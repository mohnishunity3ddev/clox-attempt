. $PSScriptRoot/_variables.ps1

$ExeFolderPath = "$BIN_DIR_PATH\$SRC_FOLDER_NAME"
Write-Output "ExeFolderPath: $ExeFolderPath"

Set-Location $ExeFolderPath
$exe = Get-ChildItem $ExeFolderPath -Recurse -Include *.exe
Write-Output "Exe: $exe"
Invoke-Expression "& `"$exe`""

goto src

# Write-Output "Chapter [$chapterArg] Project [$projectArg] FOUND!"
