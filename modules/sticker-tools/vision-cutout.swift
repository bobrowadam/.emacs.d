// Export Apple's detected foreground instances without changing the source pixels.
import Foundation
import Vision
import CoreImage

func fail(_ message: String) -> Never {
    FileHandle.standardError.write(Data((message + "\n").utf8))
    exit(1)
}

let arguments = Array(CommandLine.arguments.dropFirst())
guard arguments.count == 2 else {
    fail("Usage: swift vision-cutout.swift INPUT OUTPUT_DIRECTORY (new directory)")
}
let source = URL(fileURLWithPath: arguments[0])
let directory = URL(fileURLWithPath: arguments[1], isDirectory: true)
let files = FileManager.default
guard !files.fileExists(atPath: directory.path) else {
    fail("Refusing to overwrite an existing output directory")
}
guard #available(macOS 14.0, *) else { fail("Requires macOS 14 or later") }

do {
    let context = CIContext()
    guard let image = CIImage(contentsOf: source, options: [.applyOrientationProperty: true]),
          let pixels = context.createCGImage(image, from: image.extent) else {
        fail("Cannot decode source image")
    }
    let handler = VNImageRequestHandler(cgImage: pixels)
    let request = VNGeneratePersonInstanceMaskRequest()
    try handler.perform([request])
    guard let observation = request.results?.first, !observation.allInstances.isEmpty else {
        fail("No foreground instances detected")
    }
    try files.createDirectory(at: directory, withIntermediateDirectories: true)
    var outputs: [[String: Any]] = []
    for instance in observation.allInstances {
        let buffer = try observation.generateMaskedImage(
            ofInstances: IndexSet(integer: instance), from: handler,
            croppedToInstancesExtent: false)
        let destination = directory.appendingPathComponent("instance-\(instance).png")
        try context.writePNGRepresentation(
            of: CIImage(cvPixelBuffer: buffer), to: destination, format: .RGBA8,
            colorSpace: CGColorSpace(name: CGColorSpace.sRGB)!)
        outputs.append(["instance": instance, "path": destination.path])
    }
    let record: [String: Any] = [
        "source": source.path, "width": pixels.width, "height": pixels.height,
        "backend": "VNGeneratePersonInstanceMaskRequest", "outputs": outputs
    ]
    let data = try JSONSerialization.data(withJSONObject: record, options: [.prettyPrinted, .sortedKeys])
    try data.write(to: directory.appendingPathComponent("instances.json"), options: .withoutOverwriting)
    print(String(decoding: data, as: UTF8.self))
} catch {
    fail("Apple Vision extraction failed: \(error)")
}
