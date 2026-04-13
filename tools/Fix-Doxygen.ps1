<#
.SYNOPSIS
    Recursively scans .js files and removes "scope::" (or any XXX::) prefix 
    inside <span>XXX::something</span> patterns.

.PARAMETER Path
    Root folder to start searching (default: current directory)

.PARAMETER WhatIf
    Shows what would be changed without making changes
#>
param(
    [Parameter(Position = 0)]
    [string]$Path = ".",

    [switch]$WhatIf
)

$files = Get-ChildItem -Path $Path -Recurse -File -Include *.js,*.html -ErrorAction SilentlyContinue

if ($files.Count -eq 0) {
    Write-Host "No matching files found." -ForegroundColor DarkGray
    exit
}

foreach ($file in $files) {
    $content = Get-Content -Path $file.FullName -Raw -ErrorAction SilentlyContinue
    if (-not $content) { continue }

    $newContent = $content

    # Existing JS replacement
    if ($file.Extension -eq ".js") {
        $newContent = $newContent -replace '(?i)\[\s*"([^"]*?::)([^"]+?)"', '[ "$2"'
    } elseif ($file.Extension -eq ".html") {
        $newContent = $newContent -replace '<div class="line">...</div>', ''
        $newContent = $newContent -replace '<div class="line"><span class="preprocessor"></span>...</div>', ''
        $newContent = $newContent -replace '(<div class="line">\s*(?:<span class="keyword">end\s*</span>\s*)?)(block|associate)(\s*(?:\[[^\]]*\]|\([^)]*\)))?(</div>)', '${1}<span class="keyword">${2}</span>${3}${4}'
    }

    # Special rule only for index.html
    if ($file.Name -ieq "index.html") {
        $newContent = $newContent -replace '<div class="header">', '<div class="header" style="display:none;">'
    }

    if ($newContent -cne $content) {
        if ($WhatIf) {
            Write-Host "Would modify: $($file.FullName)" -ForegroundColor Yellow
        } else {
            Set-Content -Path $file.FullName -Value $newContent -Encoding UTF8 -NoNewline
        }
    }
}