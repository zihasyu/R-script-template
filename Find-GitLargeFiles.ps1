# 文件名: Find-GitLargeFiles.ps1

param(
    [string]$RepoPath = ".",       # Git仓库路径
    [int]$SizeThresholdMB = 20,    # 大小阈值，默认为50MiB
    [switch]$UpdateGitignore       # 是否将找到的大文件添加到.gitignore
)

# 转换为绝对路径
$RepoPath = Resolve-Path $RepoPath
$SizeThresholdBytes = $SizeThresholdMB * 1MB

Write-Host "正在分析Git仓库: $RepoPath" -ForegroundColor Cyan
Write-Host "查找大于 $SizeThresholdMB MiB 的文件..." -ForegroundColor Cyan
Write-Host "============================================================"

# 获取已被.gitignore忽略的文件模式
$gitignorePath = Join-Path $RepoPath ".gitignore"
$ignoredPatterns = @()
if (Test-Path $gitignorePath) {
    $ignoredPatterns = Get-Content $gitignorePath | 
                       Where-Object { $_ -notmatch "^\s*#" -and $_ -notmatch "^\s*$" }
}

# 查找所有大文件
$allLargeFiles = Get-ChildItem -Path $RepoPath -Recurse -File -ErrorAction SilentlyContinue | 
                 Where-Object { 
                     $_.Length -ge $SizeThresholdBytes -and 
                     -not $_.FullName.Contains("\.git\")
                 } |
                 Sort-Object Length -Descending

if ($allLargeFiles.Count -eq 0) {
    Write-Host "未找到大于 $SizeThresholdMB MiB 的文件。" -ForegroundColor Green
    exit
}

# 检查每个大文件是否已被.gitignore忽略
$unignoredLargeFiles = @()
$ignoredLargeFiles = @()

foreach ($file in $allLargeFiles) {
    $relativePath = $file.FullName.Substring($RepoPath.Length + 1).Replace("\", "/")
    $isIgnored = $false
    
    # 检查文件是否匹配任何忽略模式
    foreach ($pattern in $ignoredPatterns) {
        # 将gitignore模式转换为适合PowerShell的通配符
        $wildcard = $pattern.Replace("**/", "*").Replace("**", "*")
        
        # 检查是否是目录模式
        if ($wildcard.EndsWith("/")) {
            $wildcard = "$wildcard*"
        }
        
        # 添加文件夹路径匹配
        if (-not $wildcard.Contains("/")) {
            $altPattern = "*/$wildcard"
            if ($relativePath -like $wildcard -or $relativePath -like $altPattern) {
                $isIgnored = $true
                break
            }
        } 
        else {
            if ($relativePath -like $wildcard) {
                $isIgnored = $true
                break
            }
        }
    }
    
    if ($isIgnored) {
        $ignoredLargeFiles += $file
    } else {
        $unignoredLargeFiles += $file
    }
}

# 显示结果
Write-Host "`n已被.gitignore忽略的大文件 ($($ignoredLargeFiles.Count) 个):" -ForegroundColor Green
if ($ignoredLargeFiles.Count -gt 0) {
    $ignoredLargeFiles | ForEach-Object {
        $relativePath = $_.FullName.Substring($RepoPath.Length + 1).Replace("\", "/")
        Write-Host ("- {0,-10:N2} MB | {1}" -f ($_.Length / 1MB), $relativePath) -ForegroundColor DarkGreen
    }
}

Write-Host "`n未被.gitignore忽略的大文件 ($($unignoredLargeFiles.Count) 个):" -ForegroundColor Yellow
if ($unignoredLargeFiles.Count -gt 0) {
    $unignoredLargeFiles | ForEach-Object {
        $relativePath = $_.FullName.Substring($RepoPath.Length + 1).Replace("\", "/")
        Write-Host ("- {0,-10:N2} MB | {1}" -f ($_.Length / 1MB), $relativePath) -ForegroundColor Yellow
    }

    # 如果指定了UpdateGitignore参数，则将未忽略的大文件添加到.gitignore
    if ($UpdateGitignore) {
        Write-Host "`n正在更新.gitignore文件..." -ForegroundColor Cyan
        
        $additions = @()
        $additions += "`n# 自动添加的大文件 (>$SizeThresholdMB MB) - $(Get-Date -Format 'yyyy-MM-dd')"
        
        foreach ($file in $unignoredLargeFiles) {
            $relativePath = $file.FullName.Substring($RepoPath.Length + 1).Replace("\", "/")
            $additions += $relativePath
        }
        
        Add-Content -Path $gitignorePath -Value $additions
        Write-Host "已将 $($unignoredLargeFiles.Count) 个文件添加到.gitignore" -ForegroundColor Green
    } else {
        Write-Host "`n建议将这些文件添加到.gitignore，或使用 -UpdateGitignore 参数自动添加" -ForegroundColor Yellow
    }
}

Write-Host "`n分析完成。" -ForegroundColor Cyan