"""Local, point-guided photo cutouts and captioned WhatsApp stickers."""

import argparse
from io import BytesIO
import json
from pathlib import Path
import re

from bidi.algorithm import get_display
from PIL import Image, ImageDraw, ImageFilter, ImageFont, ImageOps


SIZE = 512
MAX_BYTES = 100_000


def coordinates(value):
    return tuple(int(part) for part in value.split(","))


def unused(path):
    if path.exists():
        raise ValueError(f"Refusing to overwrite {path}")
    path.parent.mkdir(parents=True, exist_ok=True)


def cutout(args):
    from rembg import new_session, remove

    unused(args.output)
    image = ImageOps.exif_transpose(Image.open(args.input)).convert("RGB")
    prompts = []
    if args.box:
        left, top, right, bottom = args.box
        if not (0 <= left < right <= image.width and 0 <= top < bottom <= image.height):
            raise ValueError("Subject box must lie inside the oriented source image")
        prompts.append({"type": "rectangle", "data": list(args.box)})
    for label, points in ((1, args.positive), (0, args.negative)):
        for x, y in points:
            if not (0 <= x < image.width and 0 <= y < image.height):
                raise ValueError(f"Point {(x, y)} is outside {image.size}")
            prompts.append({"type": "point", "data": [x, y], "label": label})
    session = new_session("sam", sam_quant=True, providers=["CPUExecutionProvider"])
    result = remove(image, session=session, sam_prompt=prompts)
    if args.crop:
        left, top, right, bottom = args.crop
        if not (0 <= left < right <= image.width and 0 <= top < bottom <= image.height):
            raise ValueError("Crop must lie inside the oriented source image")
        result = result.crop(args.crop)
    bounds = result.getchannel("A").getbbox()
    if bounds is None:
        raise ValueError("Segmentation returned an empty cutout")
    result.crop(bounds).save(args.output)
    print(json.dumps({"cutout": str(args.output), "size": result.crop(bounds).size}))


def visual_caption(caption, punctuation_side):
    if not caption.strip() or "\n" in caption:
        raise ValueError("Provide a nonempty, single-line caption")
    if punctuation_side == "auto":
        return get_display(caption)
    match = re.fullmatch(r"(.+?)([!?]+)", caption)
    if not match:
        raise ValueError("Explicit punctuation placement requires trailing ! or ?")
    body, punctuation = match.groups()
    body = get_display(body)
    return body + punctuation if punctuation_side == "right" else punctuation + body


def render(args):
    targets = [args.output_dir / name for name in ("sticker.png", "sticker.webp", "manifest.json", "preview.png")]
    for target in targets:
        unused(target)
    cut = ImageOps.exif_transpose(Image.open(args.input)).convert("RGBA")
    if cut.getchannel("A").getextrema()[0] == 255:
        raise ValueError("Input must be a cutout with transparency")
    bounds = cut.getchannel("A").getbbox()
    if bounds is None:
        raise ValueError("Input cutout is empty")
    visual = visual_caption(args.caption, args.punctuation_side)
    for font_size in range(70, 19, -1):
        font = ImageFont.truetype(str(args.font), font_size, layout_engine=ImageFont.Layout.BASIC)
        text_bounds = font.getbbox(visual, stroke_width=3)
        if text_bounds[2] - text_bounds[0] <= 468:
            break
    else:
        raise ValueError("Caption is too long to read at sticker size")
    text_width = text_bounds[2] - text_bounds[0]
    text_height = text_bounds[3] - text_bounds[1]
    text_top = SIZE - 20 - text_height
    subject_height = text_top - 32
    subject = ImageOps.contain(cut.crop(bounds), (464, subject_height), Image.Resampling.LANCZOS)
    layer = Image.new("RGBA", (SIZE, SIZE))
    layer.alpha_composite(subject, ((SIZE - subject.width) // 2, 24 + (subject_height - subject.height) // 2))
    outline = Image.new("RGBA", layer.size, "white")
    outline.putalpha(layer.getchannel("A").filter(ImageFilter.MaxFilter(17)))
    canvas = Image.alpha_composite(outline, layer)
    draw = ImageDraw.Draw(canvas)
    text_x = (SIZE - text_width) // 2 - text_bounds[0]
    text_y = text_top - text_bounds[1]
    draw.text((text_x, text_y), visual, font=font, fill="white", stroke_width=3, stroke_fill="black")
    png, webp, manifest, preview_path = targets
    # WebP quality changes encoding only, never the canvas size or alpha channel.
    for quality in (95, 90, 85, 80, 75, 65, 55):
        encoded = BytesIO()
        canvas.save(encoded, format="WEBP", quality=quality, method=6)
        data = encoded.getvalue()
        if len(data) < MAX_BYTES:
            break
    else:
        raise ValueError("Cannot meet the 100 KB limit at the supported quality levels")
    with Image.open(BytesIO(data)) as check:
        assert check.size == (SIZE, SIZE)
        assert check.mode == "RGBA" and check.getchannel("A").getextrema()[0] == 0
    canvas.save(png)
    webp.write_bytes(data)
    preview = Image.new("RGB", (SIZE * 2, SIZE), "#eeeeee")
    ImageDraw.Draw(preview).rectangle((SIZE, 0, SIZE * 2, SIZE), fill="#222222")
    preview.paste(canvas, (0, 0), canvas)
    preview.paste(canvas, (SIZE, 0), canvas)
    preview.save(preview_path)
    record = {
        "caption": args.caption,
        "visual_caption": visual,
        "punctuation_side": args.punctuation_side,
        "font": str(args.font),
        "source_url": args.source_url,
        "credit": args.credit,
        "license_url": args.license_url,
        "changes": "Subject cutout, crop, white outline, caption, resize, WebP encoding",
        "size": [SIZE, SIZE],
        "webp_bytes": len(data),
        "webp_quality": quality,
        "cutout": str(args.input.resolve()),
    }
    manifest.write_text(json.dumps(record, ensure_ascii=False, indent=2) + "\n")
    print(json.dumps({"png": str(png), "webp": str(webp), "manifest": str(manifest), "bytes": len(data)}))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    segment = commands.add_parser("cutout", help="Select a subject with positive and negative image points")
    segment.add_argument("--input", type=Path, required=True)
    segment.add_argument("--output", type=Path, required=True)
    segment.add_argument("--positive", type=coordinates, action="append", required=True)
    segment.add_argument("--negative", type=coordinates, action="append", default=[])
    segment.add_argument("--box", type=coordinates, help="Whole-subject left,top,right,bottom in source pixels")
    segment.add_argument("--crop", type=coordinates, help="Optional left,top,right,bottom in source pixels")
    segment.set_defaults(action=cutout)
    compose = commands.add_parser("render", help="Compose and validate a static sticker from a transparent cutout")
    compose.add_argument("--input", type=Path, required=True)
    compose.add_argument("--output-dir", type=Path, required=True)
    compose.add_argument("--caption", required=True)
    compose.add_argument("--punctuation-side", choices=("auto", "left", "right"), default="auto")
    compose.add_argument("--font", type=Path, required=True)
    compose.add_argument("--source-url", required=True)
    compose.add_argument("--credit", required=True)
    compose.add_argument("--license-url", required=True)
    compose.set_defaults(action=render)
    args = parser.parse_args()
    args.action(args)


if __name__ == "__main__":
    main()
