import Foundation

let center = DistributedNotificationCenter.default()
center.addObserver(
    forName: NSNotification.Name("com.apple.screenIsLocked"),
    object: nil,
    queue: .main
) { _ in
    let task = Process()
    task.launchPath = "/opt/homebrew/bin/gpgconf"
    task.arguments = ["--kill", "gpg-agent"]
    try? task.run()
    task.waitUntilExit()
}

center.addObserver(
    forName: NSNotification.Name("com.apple.screenIsUnlocked"),
    object: nil,
    queue: .main
) { _ in
    let task = Process()
    task.launchPath = "/opt/homebrew/bin/gpgconf"
    task.arguments = ["--launch", "gpg-agent"]
    try? task.run()
    task.waitUntilExit()
}

RunLoop.main.run()
