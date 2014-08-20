#' Helper functions for drawing networks
#'
#' These functions translate an edge list into a positioned 
#' node list, and augment the edge list with the positions 
#' in the node list.
#' @rdname Network-drawing helpers
#' @aliases edgesToVertices, edgesForPlotting
#' @param \code{.edges} KData frame giving, at a minimum, 
#' the names of the nodes arranged as an edgelist: 
#' <to> and <from> columns where each case is one edge.
#' @param \code{from} Name of the <from> variable in the edgelist
#' @param \code{to} Name of the <from> variable in the edgelist
#' @returns A data frame containing all the vertex IDs,
#' with x and y positions for each one.
#' @export
#' 
edgesToVertices <- function( .edges, from, to ) {
  vars <- structure(as.list(match.call()[-1]), class = "uneval")
  if (missing(from) || missing(to))
    stop("Must provide 'from' and 'to' variable names.")
  From <- eval( vars$from, envir=.edges )
  To   <- eval(   vars$to, envir=.edges )
  Edges <- data.frame( from=From, to=To)
  # Extract the set of vertex names from the Edges data
  Vertices <- data.frame( ID=unique(
    c(as.character(From),as.character(To)) 
    )
  )
  # get the positions automatically
  Net <- graph.data.frame( Edges )
  where <- layout.fruchterman.reingold( Net )
  # where <- layout.circle( Net )
  Vertices <- data.frame( ID=vertex.attributes(Net)$name,
                     x = where[,1], y = where[,2] )
  return( Vertices )
}

#' @param \code{Vertice} a data frame containing vertex IDs
#' and x,y coordinates for each vertex.
#' @param \code{ID} variable containing the ID of 
#' the vertices
#' @param \code{x} variable holding x-position of vertex
#' @param \code{y} variable holding y-position of vertex
#' @param \code{Edges} dataframe containing the from and to 
#' connection for each edge.  from and to should be drawn 
#' from the same set as ID in Vertices.
#' @param \code{from} variable holding the <from> IDs
#' @param \code{to} variable holding the <to> IDs
#' @export
edgesForPlotting <- function( Vertices, ID, x, y,
                              Edges, from, to) {
  
  vars <- structure(as.list(match.call()[-1]), class = "uneval")
  if (missing(from) || missing(to))
    stop("Must provide 'from' and 'to' variable names.")
  From <- eval( vars$from, envir=Edges )
  To   <- eval(   vars$to, envir=Edges )
  ID   <- eval(   vars$ID, envir=Vertices)
  X    <- eval(    vars$x, envir=Vertices )
  Y    <- eval(    vars$y, envir=Vertices )
  # Create a data frame holding the vertex ID/position info
  VPos  <- data.frame( ID=ID, X=X, Y=Y )
  # check that the vertex IDs are compatible
  if ( !all( 
        unique( c( as.character(From), 
                   as.character(To))) %in%
        Vertices$ID ) )
    stop("Vertex set doesn't match Edge set completely.")
 
  # join the x,y vertex positions to the corresponding
  # <from> and <to> IDs as <x>,<y>, <xend>, <yend>
  names(VPos) <- c(as.character(vars$from),"x","y")
  Edges <- merge( Edges, VPos, all.x=TRUE,
                       by=as.character(vars$from)) # the from locations
  names(VPos) <- c(as.character(vars$to),"xend","yend")
  Edges <- merge( Edges, VPos, all.x=TRUE,
                  by=as.character(vars$to))
  return( Edges )
}    

# An example

CellEdges <- read.csv("nciNetwork.csv")

SmallEdges <- head(CellEdges,200)

VV <- edgesToVertices( SmallEdges, 
                       from=cellLine, to=otherCellLine )
VV$width = 1:nrow(VV)
VV$color <- runif(nrow(VV))
EE <- edgesForPlotting( VV, ID=ID, x=x, y=y,
                        SmallEdges, 
                        from=cellLine, to=otherCellLine) 
VV$type <- substr(VV$ID,0,2)                       

ggplot(EE, aes(x=x,y=y)) + 
  geom_segment(size=2,
               aes(
                   xend=xend,yend=yend,
                   alpha=abs(correlation)),
               arrow=grid::arrow()) + 
  geom_point(data=VV,aes(x=x,y=y,color=type,size=size),
             size=12,fill="lightgray",alpha=.4) +
  geom_text( data=VV, aes(x=x,y=y, label=type))

