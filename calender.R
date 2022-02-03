# roundup 함수
roundUp <- function(x,to=1){
  to*(x%/%to + as.logical(x%%to))
}


get_day_fun <- function(today1){

# 올해 기준으로 할것 
  today = today()
  year = year(today) - 1
  cal <-  ymd( paste0(year,"-12-24")) + days(0:372)
  week <-  weekdays(cal) 
  lunar <- lunar.phase(as.Date(cal))


# 윤달 보정하기  
  if( cal[length(cal)]  == paste0(year+1,"-12-31")) {
    #Not Run
  } else {
    cal <- ymd( paste0(year,"-01-01")) + days(0:373)
    week <-  weekdays(cal) 
    lunar <- lunar.phase(as.Date(cal))
  }  

# 음력 데이터 구하기 
  calender <-  data.frame(
      cal, 
      week,
      lunar
    ) %>% 
      mutate(lunar_day = roundUp(lunar/ 0.212769)) 

  calender$month <- as.integer(substr(cal, 6,7))
  calender$day <- as.integer(substr(cal,9,10))
  calender$lunar_month <- NA
  
# 음력에 대한 월 처리   
  for(i in 1:nrow(calender)){
   
  # 음력월을 구한다.   
     if(calender$day[i] > calender$lunar_day[i]){
        calender$lunar_month[i] <- calender$month[i] - 1
     } else {
       calender$lunar_month[i] <- calender$month[i] - 2
     }
 
  # 음수 부분에 대해 대체 처리 한다. 
    if(calender$lunar_month[i] == -1){
       calender$lunar_month[i] <- 11 
    } else if(calender$lunar_month[i] == 0)  {
       calender$lunar_month[i] <- 12
    }
  }  

  
  # 음력을 강제로 구한다.    
  calender <-  calender %>% 
          mutate(lunar_date = as.Date(paste0(substr(cal,1,4), "-" , lunar_month, "-" ,lunar_day))) %>% 
          mutate(dif_date = as.integer(lunar_date- cal)) %>% 
          mutate(lunar_date = ifelse(dif_date > 300,
                                     paste0(year, "-" , lunar_month, "-" ,lunar_day), 
                                     paste0(substr(cal,1,4), "-" , lunar_month, "-" ,lunar_day)
                                     )) %>% 
          mutate(lunar_date = as.Date(lunar_date)) %>% 
          select(cal, lunar_date, week)
  


# 결측치에 대한 전처리 (음력 3월로 넘어갈때,  데이터의 결측치가 생긴다.)    
  for(i in 1:nrow(calender)){
    
    j = i-1
    
    if(is.na(calender$lunar_date[i])){
      calender$lunar_date[i] = calender$lunar_date[j] +1
    }  
  }
  

#음력 2021년 01월은 평달로, 작은달이며, 29일까지 있습니다.  
# 석가탄신일 계산 
  calender$inc <- "NA"
  lunar_holiday <- paste0(year+1, "-", "04-08")
  calender <- calender %>% 
    mutate(inc =  ifelse(lunar_holiday == lunar_date, "ok",NA))
  
  
  
# 명절 연휴 계산
   lunar_holiday <- c( paste0(year+1, "-", "01-01"),
                       paste0(year+1, "-", "08-15")
                       )  
  
  for(i in 1:nrow(calender)){
        j = i-1
        k = i+1
        for(a in 1:length(lunar_holiday)){
          
          lunar_temp <- lunar_holiday[a]
  
                if( calender$lunar_date[i] == lunar_temp) {
                  calender$inc[j] = "ok"
                  calender$inc[i] = "ok"
                  calender$inc[k] = "ok"
    }}}
   
# 양력 휴일 정하기 (증권 양력 휴일 정하기)   
   solar_holiday <- c(paste0(year+1, "-", "01-01"),  # 신정
                      paste0(year+1, "-", "03-01"),  #삼일절
                      paste0(year+1, "-", "05-01"),  #노동절
                      paste0(year+1, "-", "05-05"),  #어린이날
                      paste0(year+1, "-", "08-15"),  #광복절
                      paste0(year+1, "-", "10-03"),  #개천절
                      paste0(year+1, "-", "10-09"),  #한글날
                      paste0(year+1, "-", "12-25"),  #성탄절
                      paste0(year+1, "-", "12-31")   #연말 휴장일                 
   )

# 양력 휴일 계산   
   for(i in 1:nrow(calender)){
     for(a in 1:length(solar_holiday)){
       
       solar_temp <- solar_holiday[a]
       
       if( calender$cal[i] == solar_temp) {
         calender$inc[i] = "ok"
       }}}   

# Net Day 구하기 
  net_calender <- calender %>% 
        filter(!week %in% c("토요일", "일요일")) %>% 
        filter(is.na(inc))

# data 정리 하기 
  net_calender <-  net_calender %>% 
        rename(date = cal) %>% 
        select(date)  
  net_calender$n = c(1:nrow(net_calender))


# 오늘일자가 4-2일 이라고 가정 함
  n_days = (net_calender %>% filter(date == today1))$n 

  if(length(n_days)==0) {
    get_date = 1
  } else {
  get_date <- (net_calender %>% filter(n == n_days))$date
  }
 return(get_date) 
}     


  
