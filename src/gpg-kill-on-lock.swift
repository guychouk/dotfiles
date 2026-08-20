import Foundation

func runGpgconf(_ label: String, _ arguments: [String]) {
    let task = Process()
    task.launchPath = "/opt/homebrew/bin/gpgconf"
    task.arguments = arguments
    do {
        try task.run()
    } catch {
        FileHandle.standardError.write("gpg-kill-on-lock: \(label) failed to launch: \(error)\n".data(using: .utf8)!)
        return
    }
    task.waitUntilExit()
    if task.terminationStatus != 0 {
        FileHandle.standardError.write("gpg-kill-on-lock: \(label) exited \(task.terminationStatus)\n".data(using: .utf8)!)
    }
}

let center = DistributedNotificationCenter.default()
center.addObserver(
    forName: NSNotification.Name("com.apple.screenIsLocked"),
    object: nil,
    queue: .main
) { _ in
    runGpgconf("gpgconf --kill gpg-agent", ["--kill", "gpg-agent"])
}

center.addObserver(
    forName: NSNotification.Name("com.apple.screenIsUnlocked"),
    object: nil,
    queue: .main
) { _ in
    runGpgconf("gpgconf --launch gpg-agent", ["--launch", "gpg-agent"])
}

RunLoop.main.run()
