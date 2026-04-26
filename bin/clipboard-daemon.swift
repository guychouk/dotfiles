import AppKit
import Foundation

let historyPath = "/tmp/clipboard_history"
let maxEntries = 100
var history: [String] = []
var lastChangeCount: Int = NSPasteboard.general.changeCount

func escape(_ s: String) -> String {
    s.replacingOccurrences(of: "\\", with: "\\\\")
     .replacingOccurrences(of: "\n", with: "\\n")
     .replacingOccurrences(of: "\t", with: "\\t")
}

func writeHistory() {
    let content = history.map { escape($0) }.joined(separator: "\n")
    try? content.write(toFile: historyPath, atomically: true, encoding: .utf8)
}

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
    writeHistory()
}

RunLoop.main.run()
