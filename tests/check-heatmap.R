res_list <- baseLst

pal<- colorRampPalette(c("blue", "white", "orange"))(n = 16)
dev.off()
heatmap(res_list$trn_x,
        scale='none',Rowv=NA,Colv="Rowv",
        col=pal, breaks=seq(from=-1,to=15,by=1),
        margins=c(1,1),labRow=NA,labCol=NA)

dev.off()
heatmap(res_list$tst_x,
        scale='none',Rowv=NA,Colv="Rowv",
        col=pal, breaks=seq(from=-1,to=15,by=1),
        margins=c(1,1),labRow=NA,labCol=NA)
dev.off()


mean(res_list$trn_x[101:600, ])
mean(res_list$trn_x[1:100, 1:res_list$n_trn_case])
mean(res_list$trn_x[1:100, (res_list$n_trn_case +1):ncol(res_list$trn_x)])

mean(res_list$tst_x[101:600, ])
mean(res_list$tst_x[1:100, 1:res_list$n_tst_case])
mean(res_list$tst_x[1:100, (res_list$n_tst_case +1):ncol(res_list$tst_x)])

hist(apply(baseLst$trn_x[101:600, ],1,var))
hist(apply(baseLst$tst_x[101:600, ],1,var))
dev.off()
