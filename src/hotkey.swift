/**
 * Global hotkey daemon. Registers one hotkey and runs one shell command when fired.
 * Usage: hotkey <keycode> <modifiers> <command>
 *
 * Common keycodes:  B=11  P=35  Space=49
 * Carbon modifier masks: cmd=256  shift=512  option=2048  ctrl=4096
 * Example (cmd+shift+b): hotkey 11 768 /Users/you/bin/butler
 */
import AppKit
import Carbon

guard CommandLine.arguments.count == 4,
      let keyCode = UInt32(CommandLine.arguments[1]),
      let modifiers = UInt32(CommandLine.arguments[2]) else {
    fputs("usage: hotkey <keycode> <modifiers> <command>\n", stderr)
    exit(1)
}

let command = CommandLine.arguments[3]

func fire() {
    let task = Process()
    task.executableURL = URL(fileURLWithPath: "/bin/sh")
    task.arguments = ["-c", command]
    try? task.run()
}

func hotkeyCallback(
    _ callRef: EventHandlerCallRef?,
    _ event: EventRef?,
    _ userData: UnsafeMutableRawPointer?
) -> OSStatus {
    fire()
    return noErr
}

var eventSpec = EventTypeSpec(
    eventClass: OSType(kEventClassKeyboard),
    eventKind: OSType(kEventHotKeyPressed)
)
var handlerRef: EventHandlerRef?
InstallEventHandler(GetApplicationEventTarget(), hotkeyCallback, 1, &eventSpec, nil, &handlerRef)

var hotKeyRef: EventHotKeyRef?
RegisterEventHotKey(keyCode, modifiers, EventHotKeyID(signature: 0x686b6579, id: 1), GetApplicationEventTarget(), 0, &hotKeyRef)

let app = NSApplication.shared
app.setActivationPolicy(.prohibited)
app.run()
