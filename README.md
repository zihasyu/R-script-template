# R-script-template

## 可用的函数模板

### 柱状图
```r
source(here("MyR", "Bar.R"))
```

- `create_comparison_barplot` - 两柱子并排比较图

### 无点线图
```r
source(here("MyR", "Line_NoPoint.R"))
```

这些图都没有点，呈现一条曲折的线，适合性能测试使用：

- `plot_line_comparison` - 每条线上x轴都相等时可用 (推荐)
- `plot_line_comparison_xcdf` - 每条线上x轴不相等时可用，x轴会被cdf化

### 有点线图
```r
source(here("MyR", "Line_Point.R"))
```

这些图有点标记，适合每个版本测试数据分析：

- `plot_line_with_points` - 显示所有的点
- `plot_line_with_selected_points` - 只显示10个点，但线条是显示所有点的
- `plot_with_sampled_points` - 只显示10个点，线条也只显示10个点 (推荐)

$repoPath = "D:\language\RStudio\project\R-script-template"; Get-ChildItem -Path $repoPath -Recurse -File | Where-Object { $_.Length -gt 20MB -and -not $_.FullName.Contains("\.git\") } | Select-Object @{Name="大小(MB)";Expression={"{0:N2}" -f ($_.Length / 1MB)}}, @{Name="路径";Expression={$_.FullName.Substring($repoPath.Length + 1).Replace("\", "/")}} | Format-Table -AutoSize