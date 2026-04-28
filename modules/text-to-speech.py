from dotenv import load_dotenv
from elevenlabs.client import ElevenLabs
from elevenlabs.types import VoiceSettings
import json
import os
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile

load_dotenv()


def authinfo_password(host: str, user: str | None = None) -> str | None:
    """Look up a password in ~/.authinfo.gpg without writing secrets to disk."""
    authinfo = os.path.expanduser("~/.authinfo.gpg")
    if not os.path.exists(authinfo):
        return None

    try:
        proc = subprocess.run(
            ["gpg", "--quiet", "--batch", "--decrypt", authinfo],
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError:
        return None
    if proc.returncode != 0:
        return None

    for line in proc.stdout.splitlines():
        try:
            parts = shlex.split(line)
        except ValueError:
            continue
        fields = dict(zip(parts[0::2], parts[1::2]))
        if fields.get("machine") == host and (user is None or fields.get("login") == user):
            return fields.get("password")
    return None


def eleven_labs_api_key() -> str | None:
    """Resolve the ElevenLabs key from env, then authinfo.gpg."""
    return os.getenv("ELEVEN_LABS_API_KEY") or authinfo_password(
        os.getenv("ELEVEN_LABS_AUTHINFO_HOST", "api.elevenlabs.io"),
        os.getenv("ELEVEN_LABS_AUTHINFO_USER", "bob"),
    )


client = ElevenLabs(api_key=eleven_labs_api_key())

tmpdir = tempfile.mkdtemp(prefix="solveit-tts-")

def cleanup(*_):
    shutil.rmtree(tmpdir, ignore_errors=True)
    sys.exit(0)

signal.signal(signal.SIGTERM, cleanup)

input_file = sys.argv[1] if len(sys.argv) > 1 else None
if not input_file:
    print("Usage: python text-to-speech.py <json-input-file>", file=sys.stderr)
    sys.exit(1)

with open(input_file) as f:
    texts = json.load(f)
os.unlink(input_file)

print(f"TMPDIR:{tmpdir}", flush=True)  # Let Emacs know the dir to clean up

for i, text in enumerate(texts):
    tmp_path = os.path.join(tmpdir, f"chunk{i}.mp3")
    try:
        audio = client.text_to_speech.convert(
            text=text,
            voice_id="fATgBRI8wg5KkDFg8vBd",
            model_id="eleven_multilingual_v2",
            output_format="mp3_44100_128",
            voice_settings=VoiceSettings(stability=0.5, similarity_boost=0.75, speed=1.0),
        )
        with open(tmp_path, "wb") as f:
            for chunk in audio:
                f.write(chunk)
        print(tmp_path, flush=True)
    except Exception as e:
        body = getattr(e, "body", {}) or {}
        status = getattr(e, "status_code", None)
        detail = body.get("detail", {}) if isinstance(body, dict) else {}
        error_status = detail.get("status", "") if isinstance(detail, dict) else str(detail)
        if error_status == "quota_exceeded":
            print("ERROR:quota_exceeded", flush=True)
        elif status == 401:
            print("ERROR:unauthorized", flush=True)
        else:
            print(f"ERROR:{error_status or str(e)[:60]}", flush=True)
        cleanup()
        break

# tmpdir cleanup is handled by Emacs (solveit-review--cleanup deletes the dir)
