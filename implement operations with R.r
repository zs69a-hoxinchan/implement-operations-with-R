# 第一部分 #
#本部分将使用扩展包来实现运筹学相关计算
#（该部分参考文献）
#[1]高成强.R语言在几类优化问题中的应用[J].价值工程,2019,38(17):238-240.DOI:10.14018/j.cnki.cn13-1085/n.2019.17.058.
#[2]李光.基于R语言的管理运筹学教学实验设计[J].高教学刊,2020,(03):95-97+100.DOI:10.19980/j.cn23-1593/g4.2020.03.029.


### 线性规划
## 图解法，附图请看第二部分
#本部分使用扩展包lpSolve
install.packages("lpSolve")

# 第二部分 #
#本部分将不使用任何扩展包来实现运筹学相关计算

### 线性规划
## 图解法，附图
#在图解法中，我们很容易发现，最优解总是出现在可行域的顶点上，我们这时不采用单纯形法来完成这一部分代码
#以以下题目为例，我们先实现最简单的一种情况
# max z = 2x1 + 3x2
# x1 + 2x2 <= 6 
# 5x1 + 3x2 <= 15
# x1, x2 >= 0

#定义目标函数
OFV<-function(x1,x2){2*x1+3*x2}
#定义可行域对应的几条边界线（这个时候就可以发现图解法只能应用于双变量）
st1<-function(x){6-2*x}
st2<-function(x){3-3/5*x}
#计算可行域顶点
intersection<-function(st1,st2,interval) #定义求解函数
{ 
    diff_func<-function(x) st1(x)-st2(x)
    result<-uniroot(diff_func,interval=interval)
    return(result$root)
} 
interval<-c(0,6)
intersection_cov<-(intersection(st1,st2,interval));intersection_cov #求st1和st2的凸交点的x值
intersection_x1<-uniroot(st1,interval=c(0,6))$root
intersection_x2<-uniroot(st2,interval=c(0,6))$root
intersection_xaxis<-min(intersection_x1,intersection_x2);intersection_xaxis #求x轴上的顶点的x值
intersection_y1<-st1(0)
intersection_y2<-st2(0)
intersection_yaxis<-min(intersection_y1,intersection_y2);intersection_yaxis #求y轴上的顶点的x值
intersection_cov_y<-st1(intersection_cov)
vertex_list<-list(
    vertex_cov=c(intersection_cov,intersection_cov_y), # 凸交点
    vertex_xaxis = c(intersection_xaxis,0), # x轴上的顶点
    vertex_yaxis = c(0,intersection_yaxis) # y轴上的顶点
)
vertex_list
#现在我们也可以把可行域用r语言进行绘图
plot.new()
plot(0,0,type="n",xlim=c(0,6),ylim=c(0,6),xlab="x2",ylab="x1",main="ST_Region")
curve(st1,from=0,to=3,add=TRUE,col="#39c5bb",lwd=2)
curve(st2,from=0,to=5,add=TRUE,col="#39c5bb",lwd=2)
arrows(0,0,0,6,length=0.25,angle=15) #curve函数无法应用于函数st3
arrows(0,0,6,0,length=0.25,angle=15) #也可以用abline(h=0,lwd=2)和abline(v=0,lwd=2)，但是没有坐标轴箭头
polygon(c(0,intersection_xaxis,intersection_cov,0),c(0,0,intersection_cov_y,intersection_yaxis),
        col="#66ccff",border = "black")
#计算最优解
z_values<-sapply(vertex_list,function(vertex)
{
    x1<-vertex[1]
    x2<-vertex[2]
    OFV(x1,x2)
})
OPT<-max(z_values)
opt_index<-which.max(z_values)
opt_vertex<-vertex_list[[opt_index]]
print("最优解为：")
print(opt_vertex)
print("最优目标函数值为：")
print(OPT)
#将最优解对应的图像绘制到图像上
slope<--2/3
intercept<-OPT/3
abline(a=intercept,b=slope,col="#FFE212",lwd = 2)
points(opt_vertex[1],opt_vertex[2],col="#FFE212",pch = 19,cex = 1.5)
#此时可以更改之前图像的标题，但是会有很冗余的代码
#plot.new()
#plot(0,0,type="n",xlim=c(0,6),ylim=c(0,6),xlab="x2",ylab="x1",main="Optimal Solution")
#curve(st1,from=0,to=3,add=TRUE,col="#39c5bb",lwd=2)
#curve(st2,from=0,to=5,add=TRUE,col="#39c5bb",lwd=2)
#arrows(0,0,0,6,length=0.25,angle=15)
#arrows(0,0,6,0,length=0.25,angle=15)
#polygon(c(0,intersection_xaxis,intersection_cov,0),c(0,0,intersection_cov_y,intersection_yaxis),
#        col="#66ccff",border = "black")
#abline(a=intercept,b=slope,col="#FFE212",lwd = 2)
#points(opt_vertex[1],opt_vertex[2],col="#FFE212",pch = 19,cex = 1.5)