library(here)
print(here())
# 使用 here 包引用文件，更加稳健的路径管理
source(here("MyR","Bar.R"))
#create_comparison_barplot 两柱子饼图

source(here("MyR","Line_NoPoint.R"))
#这个里面的图都没有点，是一条很曲折的线，适合给性能测试使用
#plot_line_comparison 每条线上x轴都相等时可以用的 (*推荐)
#plot_line_comparison_xcdf 每条线上x轴不相等时可以用的，x轴会被cdf化

source(here("MyR","Line_Point.R"))
#这个里面的图有点，适合给perversion去测一些东西使用
#plot_line_with_points 显示所有的点
#plot_line_with_selected_points 只显示10个点，但线条是显示所有点的
#plot_with_sampled_points 只显示10个点，线条也是显示10个点的 (*推荐)
source(here("MyR","Line_Point.R"))
#plot_line*_ref() ref_line_y = xxx 即可