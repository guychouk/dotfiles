# Create directories if they don't exist
New-Item -ItemType Directory -Path "tmp/undo" -Force
New-Item -ItemType Directory -Path "pack/bundle/start" -Force

# Read the packages.txt file and process each line
Get-Content -Path "packages.txt" | ForEach-Object {
    $line = $_.Trim()
    if ($line -ne "") {
        $parts = $line -split ' '
        $repo = $parts[0]
        $dir = if ($parts.Count -gt 1) { $parts[1] } else { [System.IO.Path]::GetFileNameWithoutExtension($repo) }
        $targetPath = "pack/bundle/start/$dir"
        if (-Not (Test-Path -Path $targetPath)) {
            git -C "pack/bundle/start" clone "git@github.com:$repo.git" $dir
        }
    }
}
