#############################################################################
# R script InfluenceTables.r                                                #
# written by Tom A.B. Snijders                                              #
# January 28, 2021                                                          #
# (Gratitude to Steffen Triebel and Rene Veenstra for corrections)          #
#############################################################################

#############################################################################
#                                                                           #
# These are functions for constructing and presenting influence tables      #
# for the interpretation of results for network and behavior dynamics       #
# obtained with the RSiena program.                                         #
# Also consult the manual, Sections 13.2 and 13.4!                          #
#                                                                           #
# The main function to use is                                               #
# influenceMatrix <- function(x,xd,netname,behname,levls, levls.alt=levls)  #
# which creates a matrix containing the influence table for                 #
# siena data set xd, sienaFit or sienaMeta or sienaBayesFit object x,       #
# network variable netname (should be a character string),                  #
# dependent actor variable behname (also a character string),               #
# levels for ego levls, levels for alter levls.alt.                         #
# If levls is NULL (the default), it is taken as the integer range of       #
# the dependent actor variable.                                             #
# Mostly levls.alt will be the same as levls,                               #
# in which case it does not have to be specified.                           #
#                                                                           #
# For models utilizing creation and/or endowment effects,                   #
# by specifying include.endow=TRUE                                          #
# the sum of evaluation and endowment effects is used;                      #
# by specifying include.creation=TRUE                                       #
# the sum of evaluation and creation effects is used;                       #
# the first is relevant for maintaining, i.e., not decreasing,              #
# the values of the dependent variable,                                     #
# the second for increasing them.                                           #
#                                                                           #
# Other functions to use (see below):                                       #
# influenceTable.se                                                         #
# Computes standard error of a linear combination of elements               #
# of the influence table.                                                   #
# Not for sienaMeta or sienaBayesFit objects.                               #
#                                                                           #
# influenceTable.plot                                                       #
# Constructs a plot of the influence table using ggplot2.                   #
#                                                                           #
# If these functions are used for a sienaMeta object x,                     #
# (produced by siena08, as opposed to a sienaFit object produced by siena07)#
# xd should be one of the individual data sets used for creating x,         #
# (a single-group instead of multi-group data set)                          #
# with an average ('representative') value                                  #
# for the mean of variable <behname>.                                       #
#                                                                           #
#############################################################################

influenceTable.basis <- function(x, xd, netname, behname,
                            levls=NULL, levls.alt=levls,
                            out.ego=1, silent=FALSE, nfirst=x$nwarm+1,
                            include.endow=FALSE, include.creation=FALSE){
# Creates basic materials for a influence table for
# siena data set xd, sienaFit object x,
# network variable netname (should be a character string),
# dependent actor variable behname (also a character string),
# levels for ego levls, levels for alter levls.alt.
# For effect avAlt, levls.alt refers to average values of alter's behavior;
# for effect totAlt, levls.alt refers to total values of alter's behavior;
# For effects avSim and totSim, levls.alt refers to
#                    constant values of alter's behavior.
# For effects totSim and totAlt, out.ego is the presumed outdegree of ego
# for which the contributions to the objective function are calculated.
#
# This function can be used also for a sienaMeta object x.
# Then xd must be one of the data sets used for creating x,
# with an average ('representative') value for the mean of variable <behname>.
#
    cat("Network",netname, "; dependent behavior",behname,".\n")
    if (include.endow && include.creation)
    {
        warning(paste('It is not meaningful to include creation',
                ' and maintenance effects simultaneously.'))
    }
# Obtain estimate
    if (inherits(x, "sienaBayesFit"))
    {
        require(RSienaTest)
        theta <- RSienaTest:::sienaFitThetaTable(x, fromBayes=TRUE,
                    groupOnly=0, nfirst=nfirst)$mydf$value
        theEffects <- x$requestedEffects
        if (length(theta) != dim(theEffects)[1])
        {
            stop('mismatch between theta and effect names')
        }
# Is this correct for all model specifications?
    }
    else
    {
        theta <- x$theta
        theEffects <- x$requestedEffects
    }
# Obtain means
    if (inherits(xd, "sienaGroup"))
    {
        cat("A sienaGroup data object was given.\n")
        # Does the variable exist, and is it a dependent variable?
        thebeh <- xd[[1]]$depvars[[behname]]
        thenet <- xd[[1]]$depvars[[netname]]
    }
    else
    {
        thebeh <- xd$depvars[[behname]]
        thenet <- xd$depvars[[netname]]
    }
    if (is.null(thenet)){stop(paste('There is no network <',netname,'>.'))}
    if (is.null(thebeh)){
            stop(paste('There is no dependent behaviour variable <',behname,'>.'))}

    if (inherits(xd, "sienaGroup"))
    {
#       zmean  <- mean(sapply(xd, function(z){mean(z$depvars[[behname]], na.rm=TRUE)}))
        ztot   <- sum(sapply(xd, function(z){sum(z$depvars[[behname]], na.rm=TRUE)}))
        ztotN  <- sum(sapply(xd, function(z){sum(!is.na(z$depvars[[behname]]))}))
        zmean  <- ztot/ztotN
        zsmean <- attr(xd, "bSim")[[behname]]
        Delta  <- attr(xd, "behRange")[,behname]
        if (is.null(levls))
        {
            levls <- Delta[1] : Delta[2]
        }
        Delta  <- Delta[2] - Delta[1]
    }
    else
    {
        zmean  <- mean(colMeans(thebeh, na.rm=TRUE))
        zsmean <- attr(thebeh, 'simMean')
        Delta  <- attr(thebeh, 'range')
        if (is.null(levls))
        {
            range2 <- attr(thebeh, 'range2')
            levls <- range2[1] : range2[2]
        }
    }
    if (is.null(levls.alt))
    {
        levls.alt <- levls
    }
# perhaps attr(thebeh, 'simMean') fails if there are more than one dependent network?
# is this attribute then a vector of length >= 2?
# Note that zsmean is used only if the model includes avSim or totSim.
    replace0 <- function(k){ifelse(length(k)==0,0,k)}
    efNames <- c('linear','quad','avAlt','avSim','totAlt','totSim')
    zeff.eval <- sapply(efNames, function(s)
        {replace0(which(theEffects$name == behname &
                (theEffects$type == 'eval') &
                (theEffects$interaction1 %in% c(netname,'')) &
                theEffects$include &
                theEffects$shortName==s))})
    zeff.endow <- sapply(efNames, function(s)
        {replace0(which(theEffects$name == behname &
                (theEffects$type == 'endow') &
                (theEffects$interaction1 %in% c(netname,'')) &
                theEffects$include &
                theEffects$shortName==s))})
    zeff.creation <- sapply(efNames, function(s)
        {replace0(which(theEffects$name == behname &
                (theEffects$type == 'creation') &
                (theEffects$interaction1 %in% c(netname,'')) &
                theEffects$include &
                theEffects$shortName==s))})
# zeff gives the indicators of the effects in x
    taketheta <- function(k){ifelse(k==0,0,theta[k])}
    ztheta <- sapply(zeff.eval,taketheta)
    if (include.endow)
    {
        ztheta <- ztheta + sapply(zeff.endow,taketheta)
    }
    if (include.creation)
    {
        ztheta <- ztheta + sapply(zeff.creation,taketheta)
    }
	if (all(ztheta == 0))
	{
		cat('All parameters found for the effect of', netname, 'on',
												behname, 'are 0.\n')
	stop('there seems to be something wrong with x, xd, netname, or behname.')
	}
    if (!silent){
        cat('Parameters found are\n')
        print(ztheta[ztheta != 0.0])
        cat('\n')
        if (include.endow)
        {
            cat('This includes evaluation and endowment (maintenance) effects.\n')
        }
        if (include.creation)
        {
            cat('This includes evaluation and creation effects.\n')
        }
    }
    found <- (ztheta != 0.0)
    if (sum(found[3:6]) >= 2){
        cat("Note: more that one influence effect was found.\n")
        }
    if (found[5] | found[6]){
        cat("Note that influence in this model depends on ego's outdegree. \n")
        cat("Outdegree of ego is given as ", out.ego, "; can be changed.\n")
        }
    if (found[4] | found[6]){
        cat("Levels of alter refer to constant values of alter's behavior.\n\n")
        } else if (found[3] | found[5]){
        cat("Levels of alter refer to average values of alter's behavior.\n\n")
        }
    flush.console()
# ztheta contains the parameter values in x
    K <- length(levls)
    KA <- length(levls.alt)
    zalter <- rep(levls.alt,each=KA)
    zego <- rep(levls,K)
    fact <- 1:K
    alter <- factor(rep(fact,each=KA))
    coeffs <- matrix(NA, K*KA, length(efNames))
    coeffs[,1] <- (zego - zmean)
    coeffs[,2] <- (zego - zmean)*(zego - zmean)
    coeffs[,3] <- (zego - zmean)*(zalter - zmean)
    coeffs[,4] <- (1-(abs(zalter - zego)/Delta)-zsmean)
    coeffs[,5] <- out.ego*(zego - zmean)*(zalter - zmean)
    coeffs[,6] <- out.ego*(1-(abs(zalter - zego)/Delta)-zsmean)
    select <- coeffs %*% ztheta
    df <- data.frame(alter, zalter, zego, select=(coeffs%*%ztheta))
    list(df=df, zeff.eval=zeff.eval, zeff.endow=zeff.endow,
	     zeff.creation=zeff.creation, ztheta=ztheta, zmean=zmean, zsmean=zsmean,
         Delta=Delta, coeffs=coeffs)
}

influenceTable <- function(x, xd, netname, behname,
                                levls, levls.alt=levls, out.ego=1, ...){
# Creates a data frame containing the influence table for
# siena data set xd, sienaFit object x,
# network netname (should be a character string),
# dependent actor variable behname (also a character string),
# levels for ego levls, levels for alter levls.alt.
    df <- influenceTable.basis(x, xd, netname, behname,
                                levls, levls.alt, out.ego, ...)$df
    df$alter <- as.character(df$alter)
    df$zego <- as.numeric(as.character(df$zego))
    df$select <- as.numeric(as.character(df$select))
    df
}

influenceMatrix <- function(x, xd, netname, behname,
                                levls, levls.alt=levls, out.ego=1, ...){
# Creates a matrix containing the influence table for
# siena data set xd, sienaFit object x,
# network netname (should be a character string),
# dependent actor variable behname (also a character string),
# levels for ego levls, levels for alter levls.alt.
    stab <- influenceTable(x,xd,netname,behname,levls,levls.alt, out.ego, ...)
    mat <- matrix(as.numeric(as.character(stab$select)),
            length(unique(stab$zalter)),
            length(unique(stab$zego)), byrow=TRUE)
    colnames(mat) <- levls
    rownames(mat) <- levls.alt
    mat
}


influenceTable.se <- function(x, xd, netname, behname, levls, ww, levls.alt=levls){
# Calculates the standard error for a linear combination
# of elements of the influence table for siena data set xd, sienaFit object x,
# dependent actor variable behname (also a character string),
# network variable netname (should be a character string),
# levels for ego levls, levels for alter levls.alt.
# Coefficients of the linear combination are in matrix ww,
# which is assumed to have rows corresponding to levls and columns to levls.alt.
# The linear combination for which the standard error is computed is
# sum_{h,k} ww[h,k] * influenceTable[h,k].
    if (!all(dim(ww) == c(length(levls), length(levls.alt)))){
        stop('Dimension of ww should be the lengths of levls and levls.alt.\n')
    }
    if (class(x) == "sienaBayesFit"){
        stop('This function does not work for sienaBayesFit objects.\n')
    }
    cat("Requested cell weights (row = alter's, col = ego's behavior value):\n")
    print(cbind(which(ww != 0, arr.ind=TRUE),
            value=ww[which(ww != 0, arr.ind=TRUE)]))
    if (!all(rowSums(ww) == 0)){
        cat("Warning: not all row sums of cell weights are 0.\n Row sums are:\n")
        print(rowSums(ww))
        cat("Only contrasts are meaningful. Change weight matrix.\n\n")
    }
    inftb <- influenceTable.basis(x, xd, netname, behname, levls, levls.alt,
                silent=TRUE, ...)
    zeff.r <- inftb$zeff.eval[inftb$zeff.eval != 0] # effects included in x
    cth <- x$covtheta[zeff.r, zeff.r] # their covariance matrix
    cth[is.na(cth)] <- 0 # if any effects were fixed, they have NA in covtheta
    lincomb <- sum(as.vector(t(ww)) * inftb$df["select"])
    # the desired linear combination of cell values of the influence matrix
    wt <- colSums(as.vector(t(ww)) * inftb$coeff)
    # Vector of weights for the linear combination of parameters
    names(wt) <- names(inftb$zeff.eval)
    wt.r <- wt[names(zeff.r)] # should be restricted to what is in the model
    cat("Parameter estimates and their resulting weights are \n")
    print(rbind('param'=inftb$ztheta[inftb$zeff.eval != 0], 'weight'=wt.r))
    cat("Linear combination of cells of influence matrix", round(lincomb,4),
        "\nStandard error\n")
    print(sqrt((wt.r %*% cth %*% wt.r)[1,1]))
}

influenceTable.plot <- function(x, xd, netname, behname, levls, levls.alt=levls,
                    quad=TRUE, separation=0, bw=FALSE, labels=levls.alt, ...){
# This function is only an indication of what can be done.
# Please modify it to serve your needs.
# See below for a way to use it.
# Parameters, as far as different from above:
# quad for plotting a quadratic function (average and total alter),
# use quad=FALSE for similarity effects.
# The separation can be used to make the curves visually distinguishable
# if they overlap too much without it. An advisable value then is 0.1 or 0.2.
# labels are for the curves, corresponding to alter's average or constant values.
	require(ggplot2)
    zselect <- influenceTable(x, xd, netname, behname, levls, levls.alt, ...)
    zr <- max(zselect$select) - min(zselect$select) # only for separation
    zselect$select <- zselect$select + separation*zr*as.numeric(factor(zselect$alter))
    if (bw) {
        sp <- ggplot(zselect, aes(zego, select, group=alter, linetype=ego))
        } else {
        sp <- ggplot(zselect, aes(zego, select, group=alter, colour=alter))
        }
    if (quad) {
        gs <- geom_smooth(size=1.2, span=3)
        } else {
        gs <- geom_line(size=1.2)
        }
    if (bw) {
            sp <- sp + geom_point() + gs + scale_linetype_manual(values =
                   c('solid',  'longdash','dashed', 'twodash', 'dotdash', 'dotted'), labels=labels)
        } else {
    sp <- sp + geom_point() + gs + scale_colour_hue(labels=labels)
        }

  sp + scale_x_continuous(breaks=levls.alt, labels=labels) +
        theme(legend.key=element_blank())+
        labs(x=paste(behname),
            y=paste('evaluation function'),
            title=paste('Influence effect',netname,'on',behname),
            colour=paste(behname,'\nalter')) +
        theme_grey(base_size=26, base_family="") +
        theme(legend.key.width=unit(1, "cm")) +
        theme(plot.title=element_text(hjust=0.5))
}

# A way to use this plotting function:
# siena data set xd, sienaFit object x,
# network variable netname (should be a character string),
# dependent actor variable behname (also a character string),
# levls a range of the actor variable

# behname <- '...'
# netname <- '...'
# levls <- 1:5

# png(filename=paste("influenceTable_",behname,".png",sep=""), width=1000,height=800)
# influenceTable.plot(x, xd, netname, behname, levls, separation=0)
# graphics.off()

