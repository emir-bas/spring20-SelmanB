require(jsonlite)
require(httr)
require(data.table)
get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group9"
p_word = "asdfghjklşi"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)




#*****SELMAN*****
products<-unique(data$product_content_id)

#import the data into a list of xts objects
require(xts)
xdata<-list()
for(i in 1:length(products)){
  xdata[[products[i]]]<-xts(data[product_content_id==products[i],c(1,4:11)],
                            order.by = data[product_content_id==products[i]]$event_date)
}


#pad the data with last observed until tomorrow (CAN BE IMPROVED TO EXTRAPOLATION)
for(i in 1:8){
  l<-last(index(xdata[[i]]))
  while(l<=Sys.Date()){
    l<-l+1
    xdata[[i]]<-rbind(xdata[[i]],xts(coredata(last(xdata[[i]])),l))
  }
}



#function to generate trend component at index myindex as a linear model of length h+1 until two days before
lasttrend<-function(series,h,myindex){
  rangelast<- (myindex-2-h):(myindex-2)
  localmodel<-lm(series[as.Date(rangelast)]$sold_count ~ as.integer(index(series[as.Date(rangelast)])),na.action="na.omit")
  as.numeric(localmodel$coefficients[1]+localmodel$coefficients[2]*as.integer(myindex))
}


#generate trend component as linear model prediction of HHHH+1 observations ending two days ago
HHHH<-20 #I found 20 to be good by balancing p value and residuals on an example, and visually it is fine.
for(i in 1:8){
  xdata[[i]]<-cbind(xdata[[i]],trend=NA)
  for (j in (HHHH+3):dim(xdata[[i]])[1]){
    xdata[[i]][j,"trend"]<-lasttrend(xdata[[i]],HHHH,index(xdata[[i]][j]))
  }
}

#fit missing prices with cubic spline interpolation
for(i in 1:8){
  xdata[[i]][which(xdata[[i]][,"price"]<0),"price"]<-NA
  xdata[[i]][,"price"]<-na.spline(xdata[[i]][,"price"])
}



