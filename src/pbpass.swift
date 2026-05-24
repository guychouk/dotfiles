// Copies stdin to clipboard with ConcealedType flag.
// Password managers (and clippy) respect this and don't store it.

import AppKit
import Foundation

let password = FileHandle.standardInput.readDataToEndOfFile()
guard let password_str = String(data: password, encoding: .utf8)?.trimmingCharacters(in: .newlines) else { exit(1) }

let pb = NSPasteboard.general
pb.clearContents()
pb.setString(password_str, forType: .string)
pb.setString("", forType: NSPasteboard.PasteboardType("org.nspasteboard.ConcealedType"))
