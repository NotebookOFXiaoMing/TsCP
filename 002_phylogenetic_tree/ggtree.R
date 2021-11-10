library(ggtree)
library(ggstar)
library(treeio)
library(ggplot2)
library(stringr)

tree<-read.tree("cp.output.fasta.treefile")


ggtree(tree)+
  geom_tiplab()+
  xlim(0,0.7)+
  geom_star(aes(x=0.7,y=25),
            size=5,
            fill="black") -> p1


new_df<-ggplot_build(p1)$data[[3]] %>% 
  dplyr::select(x,y,label)

new_df$label<-str_replace(new_df$label,"NC_","NC")

new_df$label1<-matrix(unlist(str_split(new_df$label,"_")),ncol = 3,byrow=T)[,1]
new_df$label2<-paste(matrix(unlist(str_split(new_df$label,"_")),ncol = 3,byrow=T)[,2],
                      matrix(unlist(str_split(new_df$label,"_")),ncol = 3,byrow=T)[,3],
                      sep=" ")
new_df$label1<-str_replace(new_df$label1,"NC","NC_")

pdf(file = "tree.pdf",
    width = 8,
    height = 10,
    family = "serif")
ggtree(tree)+
  xlim(0,0.7)+
  geom_text(data=new_df,
            aes(x=x,y=y,label=label1),
            hjust=0)+
  geom_text(data=new_df,
            aes(x=x+0.08,y=y,
                label=label2),
            hjust=0,
            fontface="italic")+
  geom_star(aes(x=0.65,y=25),size=5,fill="black")
dev.off()
