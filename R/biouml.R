biouml.get <- function(path)
{
  query  <- function(serverPath) queryJSON(serverPath, params=c(de=path))$values
  tableInfo <- query("/web/table/columns")
  tableData <- query("/web/table/rawdata")
  
  names(tableData) <- sapply(tableInfo, function(colInfo) colInfo['name']);
  list.to.data.frame.fast(tableData[-1], tableData[[1]])
}

list.to.data.frame.fast <- function(a, rowNames)
{
  class(a) <- 'data.frame'
  if(!missing(rowNames))
    attr(a, 'row.names') <- rowNames
  a
}

biouml.query <- function(serverPath, params=character(), method='get')
{
  con <- getConnection()
  opts <- curlOptions( httpheader=paste("Cookie: JSESSIONID=", con$sessionId, sep='') )
  url <- paste(con$url, serverPath, sep='')
  if(method=='get')
  {
    if(length(params) != 0)
      url <- paste(url, paste(curlEscape(names(params)), curlEscape(params), sep='=',collapse='&' ), sep='?')
    rawToChar(getBinaryURL(url, .opts=opts))
  }
  else if(method=='post')
  {
    postForm(url, .params=params, .opts=opts)
  }
  else stop("Unknown method, only get and post allowed")
}

queryJSON <- function(serverPath, params=list(), method='get', simplify=T, reconnect=T)
{
  content <- biouml.query(serverPath, params, method)
  json <- fromJSON(content, simplify=simplify, asText=T)
  responseType <- as.numeric(as.list(json)$type)
  if( responseType == 3 && reconnect)
  {
    con <- biouml.reconnect(con)
    return(queryJSON(serverPath, params, method, simplify, reconnect=F))
  }
  else if(responseType != 0)
  {
    stop(as.list(json)$message)
  }
  json
}

getConnection <- function()
{
  con <- getOption('biouml_connection')
  if(is.null(con)) stop("Not logged in to biouml, run biouml.login() first")
  con
}

biouml.login <- function(user='', pass='', url='http://localhost:8080/biouml')
{
  invisible(biouml.reconnect(list(url=url, user=user, pass=pass)))
}

biouml.reconnect <- function(con)
{
  header <- basicHeaderGatherer()
  content <- basicTextGatherer()
  opts <- curlOptions(headerfunction=header$update, writefunction=content$update)
  postForm(paste(con$url, "/web/login", sep=''), username=con$user, password=con$pass, .opts=opts)
  contentJson <- fromJSON(content$value())
  if(contentJson$type != 0)
    stop(contentJson$message);
  con$sessionId <- sub('JSESSIONID=([^;]+).*', '\\1', header$value()['Set-Cookie'])
  options(biouml_connection=con)
  con
}

biouml.logout <- function()
{
  queryJSON('/web/logout')
  return()
}

next.job.id <- function()
{
  jobID <- getOption("biouml_last_job_id", 1L)
  options(biouml_last_job_id=jobID+1L)
  paste('RJOB', as.numeric(strftime(Sys.time(), "%OS6"))*1e6, jobID, sep='')
}

biouml.analysis <- function(analysisName, parameters=list(), wait=T, verbose=T)
{
  jobID <- next.job.id()
  parameters <- as.name.value( as.tree( parameters ) )
  queryJSON("/web/analysis", params=c(jobID=jobID, de=analysisName, json=toJSON(parameters)), method='post')
  if(wait) biouml.job.wait(jobID, verbose)
  jobID
}

biouml.job.info <- function(jobID)
{
  info <- queryJSON("/web/jobcontrol", params=c(jobID=jobID), method='post', simplify=F)
  info$status <- c('CREATED','RUNNING', 'PAUSED', 'COMPLETED', 'TERMINATED_BY_REQUEST', 'TERMINATED_BY_ERROR')[info$status+1L]
  info
}

biouml.job.wait <- function(jobID, verbose=T)
{
  messageLength <- 0
  while(T)
  {
    info <- biouml.job.info(jobID)
    if(verbose)
    {
      if(!is.null(info$percent)) cat(info$percent, '%\n')
      if(!is.null(info$values))
      {
        cat(substring(info$values, messageLength + 1L))
        messageLength <- nchar(info$values)
      }
    }
    if(info$status %in% c('COMPLETED', 'TERMINATED_BY_REQUEST', 'TERMINATED_BY_ERROR'))
      return(info)
    Sys.sleep(1);
  }
}

as.tree <- function(values)
{
  create.hierarchy <- function(path, value)
    Reduce(function(name, val) {res <- list(); res[[name]] <- val; res}, path, value, right=T)
  add <- function(tree, path, value) {
    if(length(path) == 0) return(value);
    child <- tree[[ path[1] ]]
    if( is.list(child) )
      tree[[ path[1] ]] <- add( child, path[-1], value )
    else
      tree[[ path[1] ]] <- create.hierarchy(path[-1], value)
    tree
  }
  Reduce(function(tree, e) add(tree, strsplit(e, '/')[[1]], values[[e]]), names(values), list())
}

as.name.value <- function(tree)
{
  if(!is.list(tree)) return(tree)
  lapply(seq_along(tree), function(i) list( name=names(tree)[i], value=as.name.value(tree[[i]]) ) )
}

biouml.put <- function(path, value)
{
  biouml.type <- function(val)
    if(is.integer(val)) "Integer"
    else if(is.numeric(val)) "Float"
    else if(is.logical(val)) "Boolean"
    else if(is.character(val) || is.factor(val)) "Text"
    else stop("Can not put to biouml column of type ", class(val))

  columns <- list()
  columns[[1]] <- list(name='ID', type='Text') 
  for(i in seq_len(ncol(value))) columns[[ i + 1 ]] <- list(name=colnames(value)[i], type=biouml.type(value[,i]))

  res <- list()
  res[[1]] <- row.names(value)
  for(i in seq_len(ncol(value))) res[[ i + 1 ]] <- value[,i]

  queryJSON("/web/table/createTable", params=c(de=path, columns=toJSON(columns), data=toJSON(res)))
  return()
}

biouml.export <- function(path, exporter="Tab-separated text (*.txt)", exporter.params=list(), target.file="biouml.out")
{
  exporter.params <- as.name.value(as.tree(exporter.params))
  content <- biouml.query("/web/export", params=list(exporter=exporter, type="de", detype="Element", de=path, parameters=toJSON(exporter.params)))
  cat(content, file=target.file)
}

biouml.import <- function(file, parentPath, importer, importer.params=list())
{
  fileID <- next.job.id();
  jobID <- next.job.id();
  params <- list(fileID=fileID, file=fileUpload(file))
  biouml.query("/web/upload", params=params, method='post')
  importer.params <- as.name.value(as.tree(importer.params))
  params=list(type="import", de=parentPath, fileID=fileID, jobID=jobID,
              format=importer, json=toJSON(importer.params))
  queryJSON("/web/import", params=params)
  biouml.job.wait(jobID)$results[[1]]
}
