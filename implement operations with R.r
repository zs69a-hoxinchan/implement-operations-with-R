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
cat("最优解为：",opt_vertex,"\n","最优目标函数值为：",OPT,"\n")
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

##单纯形法
#单纯形法对线性代数进行求解，需要进行标准化，构造初始单纯形表后迭代更新
#以以下题目为例，我们用单纯形法对其进行最优解的求解
# max z = 50x1 + 100x2
# x1 + x2 + s1 = 300
# 2x1 + x2 + s2 = 400
# x2 + s3 = 250
# x1,x2,s1,s2,s3 >= 0 

rm(list = ls()) #清空工作区
#定义目标函数系数
z<-c(x1=50,x2=100,s1=0,s2=0,s3=0)
#构造系数矩阵
coef_matrix<-matrix(c(1, 1, 1, 0, 0,
                      2, 1, 0, 1, 0,
                      0, 1, 0, 0, 1), 
                   nrow = 3, byrow = TRUE)
colnames(coef_matrix)<-c("x1","x2","s1","s2","s3");coef_matrix
#初始化基变量与系数（因为我们已经做完标准化处理，因此必然存在着以松弛变量系数构建而成的单位矩阵作为可行基）
basis<-coef_matrix[,c("s1","s2","s3")];basis #基变量
not_b<-coef_matrix[,c("x1","x2")];not_b #非基变量
initial_z<-rep(0,ncol(coef_matrix));initial_z #初始化系数
#构造单纯形表格（增广矩阵）
cor_coef<-c(z[3:5]);cor_coef #构造由基变量对应的目标函数系数的向量
constant<-c(300,400,250)
interaction_count<-0 #初始化迭代次数
while(TRUE){
    print(paste("第",interaction_count,"次迭代"))
    interaction_count<-interaction_count+1
    #计算中间量和检验数并判断是否达到最优解
    zj<-c() 
    interme<-c() #初始化循环中间量
    for( i in seq_len(ncol(coef_matrix))){
        zj<-c(cor_coef%*%coef_matrix[,i]) #将cor_coef与系数矩阵的每一列进行矩阵乘法
        interme<-c(interme,zj) #将结果存储进zij向量
    };interme
    reduced_costs<-c(z-interme);reduced_costs #获得检验数对应的向量
    if(max(reduced_costs)<=0){
        print("达到最优解")
        #输出最优解和目标函数最大值
        opt_vertex<-rep(0,length(z))
        names(opt_vertex)<-names(z)
        for (i in seq_len(ncol(basis))){
            opt_vertex[colnames(basis)[i]] <- constant[i]
        }
        # 计算目标函数的最大值
        OPT<-sum(opt_vertex*z)
        # 输出最优解和目标函数的最大值
        for(var in names(opt_vertex)){
            print(paste(var,"的最优解是",opt_vertex[var]))
        }
        print(paste("目标函数的最大值为：",OPT))
        break
    } else {
        print("未达到最优解，进行迭代")
    }
    #单纯形法迭代部分
    enter_col<-which.max(reduced_costs) 
    enter<-coef_matrix[,enter_col,drop = FALSE] #选择入基变量
    valid_indices<-which(enter>0) #避免除数为0的情况
    if(length(valid_indices)==0){
        stop("无界解")
    }
    ratio_cons<-constant[valid_indices]/enter[valid_indices]
    exit_col<-valid_indices[which.min(ratio_cons)] 
    exit<-basis[,exit_col,drop = FALSE] #选择出基变量
    pivot<-coef_matrix[exit_col,enter_col] #确定主元
    #更新基变量与系数   
    not_b<-not_b[,!colnames(not_b)%in%colnames(enter),drop=FALSE]
    not_b<-cbind(not_b,basis[,exit_col,drop=FALSE]) #更新非基变量
    basis[,exit_col]<-enter
    colnames(basis)[exit_col]<-colnames(enter) #更新基变量
    #高斯消元
    coef_matrix[exit_col,]<-coef_matrix[exit_col,]/pivot
    constant[exit_col]<-constant[exit_col]/pivot #归一化主元所在行
    cor_coef<-z[colnames(basis)] #更新基变量对应的系数
    for(i in seq_len(nrow(coef_matrix))){
        if(i != exit_col){
            factor<-coef_matrix[i,enter_col] #获取了当前行中入基变量对应的系数，以保证常数项正常更新
            coef_matrix[i,]<-coef_matrix[i,]- #因为vscode的原因句子太长有蓝色波浪线故拆分
              factor*coef_matrix[exit_col,]
            constant[i]<-constant[i]-
              factor*constant[exit_col] #更新常数项（之前因为先更新系数矩阵导致常数项无法更新debug了半天）
        }
    } 
}

###特殊线性规划
##工商管理问题
#在人力分配问题上，我们会遇到与排班相关的问题，以以下题目为例，我们将实现他们:
# 一家中型的百货商场对售货员的需求经过统计分析如表4-2所示。
# 所需售货员人数表
# 时间          | 星期一  | 星期二  | 星期三 | 星期四 | 星期五  | 星期六 | 星期日
# 所需售货员人数 |   15   |   24   |   25   |   19   |   31   |   28   |   28
#为了保证售货员充分休息，要求售货员每周工作五天，休息两天，并要求休息的两天是连续的。
#问应该如何安排售货员的休息日期，既满足工作需要，又使配备的售货员的人数最少？
#此时我们需要使用贪心算法







##运输问题
