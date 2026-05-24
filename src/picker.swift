/**
 * Stdin-to-stdout fuzzy picker. Reads lines from stdin, shows a native
 * search-filtered list, prints the selected line to stdout.
 * Enter to confirm, Escape or close to cancel.
 */
import AppKit

var inputLines: [String] = []
while let line = readLine() {
    let s = line.trimmingCharacters(in: .whitespacesAndNewlines)
    if !s.isEmpty { inputLines.append(s) }
}

class KeyWindow: NSWindow {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
}

class Picker: NSObject, NSApplicationDelegate, NSWindowDelegate,
              NSTableViewDelegate, NSTableViewDataSource, NSTextFieldDelegate {

    var window: NSWindow!
    var searchField: NSTextField!
    var tableView: NSTableView!
    var all: [String] = inputLines
    var shown: [String] = inputLines

    func applicationDidFinishLaunching(_: Notification) {
        window = KeyWindow(
            contentRect: NSRect(x: 0, y: 0, width: 680, height: 400),
            styleMask: [.borderless, .resizable],
            backing: .buffered,
            defer: false
        )
        window.isOpaque = false
        window.backgroundColor = .clear
        window.isMovableByWindowBackground = true
        window.delegate = self
        if let screen = NSScreen.main {
            let wf = window.frame
            let sf = screen.visibleFrame
            let x = sf.midX - wf.width / 2
            let y = sf.midY - wf.height / 2 + sf.height * 0.1
            window.setFrameOrigin(NSPoint(x: x, y: y))
        }

        let blur = NSVisualEffectView()
        blur.material = .hudWindow
        blur.blendingMode = .behindWindow
        blur.state = .active
        blur.wantsLayer = true
        blur.layer?.cornerRadius = 14
        blur.layer?.masksToBounds = true
        window.contentView = blur

        // Search field
        let search = NSTextField()
        search.placeholderAttributedString = NSAttributedString(
            string: "Search...",
            attributes: [.foregroundColor: NSColor.white.withAlphaComponent(0.35),
                         .font: NSFont.systemFont(ofSize: 22, weight: .light)]
        )
        search.isBordered = false
        search.drawsBackground = false
        search.font = NSFont.systemFont(ofSize: 22, weight: .light)
        search.textColor = .white
        search.focusRingType = .none
        search.translatesAutoresizingMaskIntoConstraints = false
        search.delegate = self
        blur.addSubview(search)
        searchField = search

        let sep = NSBox()
        sep.boxType = .separator
        sep.translatesAutoresizingMaskIntoConstraints = false
        blur.addSubview(sep)

        let scroll = NSScrollView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.hasVerticalScroller = true
        scroll.autohidesScrollers = true
        scroll.drawsBackground = false
        blur.addSubview(scroll)

        let table = NSTableView()
        table.backgroundColor = .clear
        table.enclosingScrollView?.drawsBackground = false
        let col = NSTableColumn(identifier: .init("item"))
        col.title = ""
        table.addTableColumn(col)
        table.headerView = nil
        table.rowHeight = 40
        table.delegate = self
        table.dataSource = self
        table.doubleAction = #selector(confirm)
        table.target = self
        table.style = .plain
        scroll.contentInsets = NSEdgeInsets(top: 8, left: 0, bottom: 0, right: 0)
        scroll.documentView = table
        tableView = table

        NSLayoutConstraint.activate([
            search.topAnchor.constraint(equalTo: blur.topAnchor, constant: 18),
            search.leadingAnchor.constraint(equalTo: blur.leadingAnchor, constant: 22),
            search.trailingAnchor.constraint(equalTo: blur.trailingAnchor, constant: -22),
            search.heightAnchor.constraint(equalToConstant: 36),
            sep.topAnchor.constraint(equalTo: search.bottomAnchor, constant: 14),
            sep.leadingAnchor.constraint(equalTo: blur.leadingAnchor),
            sep.trailingAnchor.constraint(equalTo: blur.trailingAnchor),
            scroll.topAnchor.constraint(equalTo: sep.bottomAnchor, constant: 4),
            scroll.leadingAnchor.constraint(equalTo: blur.leadingAnchor),
            scroll.trailingAnchor.constraint(equalTo: blur.trailingAnchor),
            scroll.bottomAnchor.constraint(equalTo: blur.bottomAnchor),
        ])

        selectFirst()
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func selectFirst() {
        guard !shown.isEmpty else { return }
        tableView.selectRowIndexes(.init(integer: 0), byExtendingSelection: false)
    }

    // MARK: NSTextFieldDelegate

    func controlTextDidBeginEditing(_ obj: Notification) {
        guard let editor = obj.userInfo?["NSFieldEditor"] as? NSTextView else { return }
        editor.textColor = .white
        editor.insertionPointColor = .white
    }

    func controlTextDidChange(_: Notification) {
        let q = searchField.stringValue.lowercased()
        shown = q.isEmpty ? all : all.filter { $0.lowercased().contains(q) }
        tableView.reloadData()
        selectFirst()
    }

    func control(_: NSControl, textView _: NSTextView, doCommandBy sel: Selector) -> Bool {
        switch sel {
        case #selector(NSResponder.insertNewline(_:)):
            confirm(); return true
        case #selector(NSResponder.cancelOperation(_:)):
            exit(0)
        case #selector(NSResponder.moveDown(_:)):
            let i = min(tableView.selectedRow + 1, shown.count - 1)
            tableView.selectRowIndexes(.init(integer: i), byExtendingSelection: false)
            tableView.scrollRowToVisible(i)
            return true
        case #selector(NSResponder.moveUp(_:)):
            let i = max(tableView.selectedRow - 1, 0)
            tableView.selectRowIndexes(.init(integer: i), byExtendingSelection: false)
            tableView.scrollRowToVisible(i)
            return true
        default:
            return false
        }
    }

    // MARK: NSTableViewDataSource

    func numberOfRows(in _: NSTableView) -> Int { shown.count }

    func tableView(_: NSTableView, viewFor _: NSTableColumn?, row: Int) -> NSView? {
        let cellView = NSTableCellView()
        let label = NSTextField(labelWithString: shown[row])
        label.font = NSFont.systemFont(ofSize: 16, weight: .regular)
        label.textColor = .white
        label.translatesAutoresizingMaskIntoConstraints = false
        cellView.addSubview(label)
        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: cellView.leadingAnchor, constant: 16),
            label.centerYAnchor.constraint(equalTo: cellView.centerYAnchor),
            label.trailingAnchor.constraint(equalTo: cellView.trailingAnchor, constant: -16),
        ])
        return cellView
    }

    func tableView(_ tv: NSTableView, rowViewForRow _: Int) -> NSTableRowView? {
        let row = NSTableRowView()
        return row
    }

    // MARK: NSWindowDelegate

    func windowDidBecomeKey(_: Notification) {
        window.makeFirstResponder(searchField)
    }

    func windowWillClose(_: Notification) { exit(0) }

    // MARK: Actions

    @objc func confirm() {
        let row = tableView.selectedRow
        if row >= 0 && row < shown.count { print(shown[row]) }
        exit(0)
    }
}

let app = NSApplication.shared
app.setActivationPolicy(.regular)
let delegate = Picker()
app.delegate = delegate
app.run()
