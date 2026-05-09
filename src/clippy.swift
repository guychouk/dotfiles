/**
* Clipboard history daemon and picker. Single binary with two modes:
* - `--daemon`: watches clipboard and stores entries to disk (sensitive stuff omitted)
* - `--list`: displays history and pipes selection to choose
*/

import AppKit
import Foundation

let maxEntries = 100
let historyDir = "/tmp/clipboard_history"

func runDaemon() {
    var history: [String] = []
    var lastChangeCount: Int = NSPasteboard.general.changeCount

    Timer.scheduledTimer(withTimeInterval: 0.5, repeats: true) { _ in
        let changeCount = NSPasteboard.general.changeCount
        guard changeCount != lastChangeCount else { return }
        lastChangeCount = changeCount
        let concealedType = NSPasteboard.PasteboardType("org.nspasteboard.ConcealedType")
        guard NSPasteboard.general.types?.contains(concealedType) != true else { return }
        guard let text = NSPasteboard.general.string(forType: .string) else { return }
        guard !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else { return }
        history.removeAll { $0 == text }
        history.insert(text, at: 0)
        if history.count > maxEntries {
            history = Array(history.prefix(maxEntries))
        }
        try? FileManager.default.createDirectory(atPath: historyDir, withIntermediateDirectories: true)
        for (index, entry) in history.enumerated() {
            let path = "\(historyDir)/\(index)"
            try? entry.write(toFile: path, atomically: true, encoding: .utf8)
        }
    }

    RunLoop.main.run()
}

func runList() {
    let fm = FileManager.default
    guard let files = try? fm.contentsOfDirectory(atPath: historyDir) else { exit(0) }

    let sorted = files.sorted { Int($0) ?? -1 < Int($1) ?? -1 }
    guard !sorted.isEmpty else { exit(0) }

    let chooseProcess = Process()
    chooseProcess.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/choose")

    let inputPipe = Pipe()
    let outputPipe = Pipe()
    chooseProcess.standardInput = inputPipe
    chooseProcess.standardOutput = outputPipe

    try? chooseProcess.run()

    for file in sorted {
        let path = "\(historyDir)/\(file)"
        guard let content = try? String(contentsOfFile: path, encoding: .utf8) else { continue }

        let firstLine = content.split(separator: "\n", maxSplits: 1, omittingEmptySubsequences: false).first.map { String($0) } ?? ""
        let preview = String(firstLine.prefix(80))
        let line = "\(file) │ \(preview)\n"

        if let data = line.data(using: .utf8) {
            inputPipe.fileHandleForWriting.write(data)
        }
    }
    inputPipe.fileHandleForWriting.closeFile()

    chooseProcess.waitUntilExit()

    let selectedData = outputPipe.fileHandleForReading.readDataToEndOfFile()
    guard let selected = String(data: selectedData, encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines), !selected.isEmpty else { exit(0) }

    let filename = selected.split(separator: " ").first.map { String($0) } ?? ""

    let path = "\(historyDir)/\(filename)"
    guard let content = try? String(contentsOfFile: path, encoding: .utf8) else { exit(1) }

    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(content, forType: .string)
}

if CommandLine.arguments.count <= 1 {
    fputs("Usage: clipboard-daemon [--daemon|--list]\n", stderr)
    exit(1)
}

switch CommandLine.arguments[1] {
    case "--daemon":
        runDaemon()
    case "--list":
        runList()
    default:
        fputs("Usage: clipboard-daemon [--daemon|--list]\n", stderr)
        exit(1)
}
