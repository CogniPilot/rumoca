[CmdletBinding()]
param(
    [string]$Version = $env:RUMOCA_INSTALL_VERSION,
    [string]$Repo = $(if ($env:RUMOCA_INSTALL_REPO) { $env:RUMOCA_INSTALL_REPO } else { "climamind/rumoca" }),
    [string]$BinDir = $(if ($env:RUMOCA_INSTALL_BIN_DIR) { $env:RUMOCA_INSTALL_BIN_DIR } else { Join-Path $HOME ".rumoca\bin" }),
    [switch]$WithLsp
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Version)) {
    $Version = "latest"
}

if (-not $IsWindows) {
    throw "install.ps1 is for Windows. Use install.sh on Linux/macOS."
}

$osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString().ToLowerInvariant()
$arch = switch ($osArch) {
    "x64" { "x86_64" }
    "arm64" { "aarch64" }
    default { throw "unsupported architecture: $osArch" }
}

if ($arch -ne "x86_64") {
    throw "no Windows release asset available for architecture '$arch' yet"
}

if ($Version -eq "latest") {
    $apiUrl = "https://api.github.com/repos/$Repo/releases/latest"
    $release = Invoke-RestMethod -Uri $apiUrl
    if (-not $release.tag_name) {
        throw "failed to resolve latest release tag from $apiUrl"
    }
    $tag = [string]$release.tag_name
}
elseif ($Version.StartsWith("v")) {
    $tag = $Version
}
else {
    $tag = "v$Version"
}

$rumocaAsset = "rumoca-windows-$arch.exe"
$lspAsset = "rumoca-lsp-windows-$arch.exe"

New-Item -ItemType Directory -Path $BinDir -Force | Out-Null

function Install-RumocaAsset {
    param(
        [Parameter(Mandatory = $true)][string]$Asset,
        [Parameter(Mandatory = $true)][string]$OutputName
    )

    $url = "https://github.com/$Repo/releases/download/$tag/$Asset"
    $tmp = New-TemporaryFile
    try {
        Write-Host "Downloading $url"
        Invoke-WebRequest -Uri $url -OutFile $tmp.FullName
        Copy-Item -Path $tmp.FullName -Destination (Join-Path $BinDir $OutputName) -Force
    }
    finally {
        Remove-Item -Path $tmp.FullName -Force -ErrorAction SilentlyContinue
    }
}

Install-RumocaAsset -Asset $rumocaAsset -OutputName "rumoca.exe"
if ($WithLsp.IsPresent) {
    Install-RumocaAsset -Asset $lspAsset -OutputName "rumoca-lsp.exe"
}

Write-Host "Installed rumoca to $(Join-Path $BinDir "rumoca.exe")"
if ($WithLsp.IsPresent) {
    Write-Host "Installed rumoca-lsp to $(Join-Path $BinDir "rumoca-lsp.exe")"
}

if (($env:Path -split ";") -notcontains $BinDir) {
    Write-Host "Add '$BinDir' to your PATH to use rumoca from any terminal."
}

& (Join-Path $BinDir "rumoca.exe") --version
