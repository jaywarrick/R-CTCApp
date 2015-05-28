source('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images/ImageViewer.R')
require(shiny)
source('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images/server.R');
source('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images/ui.R');

runApp(shinyApp(ui=getUI(), server=getServer()))#, launch.browser=FALSE);
# runApp(appDir='/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images', launch.browser=T)


library(Rserve)
Rserve(args='--no-save --RS-encoding "utf" --RS-enable-control')

library(RSclient);
c <- RSconnect();
duh <- RSserverEval(c,"xx<-1");
RSserverEval(c,"source('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images/ui.R')");
RSserverEval(c,"source('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/Images/server.R')");
RSserverEval(c, "xx <- runApp(shinyApp(ui=getUI(), server=getServer()), launch.browser = TRUE)")
RSclose(c)
while(myTempDuh)
c1 <- RSconnect();
RSeval(c1, "bb <- 2")
myTempDuh <- RSeval(c1, quote(yy<-xx))
RSclose(c1)

