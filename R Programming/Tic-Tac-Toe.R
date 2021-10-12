## The board

board <- matrix(NaN,nrow = 3,ncol = 3)

## rounds

v <- c("ROUND 1", "ROUND 2", "ROUND 3","ROUND 4","ROUND5")



#identify functions#

# 1- function to get x or o

input_symbol <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  Sys.sleep(0.5)
  cat("X or O? ")
  symbol <<- readLines(con = con, n = 1)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2- function to get row number

input_row <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  
  Sys.sleep(0.5)
  cat("which row ? ")
  row_t <<- scan(con ,integer(),n=1,quiet = T)
  while (row_t>3|row_t<1) {
    print("invalid please choose 1,2 or 3")
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    Sys.sleep(0.5)
    cat("which row ? ")
    row_t <<- scan(con ,integer(),n=1,quiet = T)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#3- function to get col number




input_col <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  Sys.sleep(0.5)
  cat("which column ")
  col_t <<- scan(con ,numeric(),n=1,quiet = T)
  
  
  while (col_t>3|col_t<1) {
    
    print("Invalid Please ckoose 1,2 or 3")
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    Sys.sleep(0.5)
    cat("which column ")
    col_t <<- scan( con, integer(),n=1,quiet = T)
    
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 4- function to get y or n


input_confirm <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  confirmation<<- readLines(con = con, n=1)
  
  while (confirmation!="y"&confirmation!="n") {
    cat("invalid!, Please enter y or n  ")
    
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    confirmation<<- readLines(con = con, n = 1)
  } 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# getting x or o


input_symbol()

while (symbol!="x"&symbol!="o") {
  print("invalid! Please, enter either x or o only")
  input_symbol()
}


###### if user start first


if (symbol=="x") {
  
  
  
  
  
  repeat{
    for (i in v) {
      print(paste("#####################################",i,"#######################################"),quote = F)#,justify = "left",right=T)
      
      
      Sys.sleep(1)
      print("current board :",quote=F)
      cat("......................................................",fill =T)
      Sys.sleep(1)
      print(board)
      Sys.sleep(1)
      
      print( "palyer's x turn:",quote=F)
      Sys.sleep(1)
      
      ## get row      
      
      
      input_row()
      
      
      
      ## get col  
      
      
      input_col()
      
      ## to make sure that x put in empty square  
      
      while (board[row_t,col_t]!="NaN") {
        print("Invalid! this place is already taken")
        
        input_row()
        
        
        
        
        
        
        input_col()
      }
      
      #### to get y or n  
      
      
      
      cat("Are you sure you want to put it on row ",row_t)
      cat(" and col ",col_t)
      cat(" ?")
      cat("[ y/n]")
      
      input_confirm()
      
      
      
      if(confirmation=="y"){
        board[row_t,col_t]<- "x"
        print("current board :",quote=F)
        cat("......................................................",fill =T)
        Sys.sleep(1)
        print(board)
        
        
        ## to get the position of empty squares and and their number     
        
        e <<-which(board == "NaN")
        
        w <- length(e)
        
        
        
        
        
        ## to test if x win
        
        if (board[1,1]==board[1,2]&board[1,2]==board[1,3]&board[1,3]==symbol){
          cat("player",symbol,"win")
          
          break
        } else if (board[2,1]==board[2,2]&board[2,2]==board[2,3]&board[2,3]==symbol) {
          cat("player",symbol,"win")
          break
        } else if(board[3,1]==board[3,2]&board[3,2]==board[3,3]&board[3,3]==symbol){
          cat("player",symbol,"win")
          
          break
        } else if(board[1,1]==board[2,1]&board[2,1]==board[3,1]&board[3,1]==symbol){
          cat("player",symbol,"win")
          break
        }else if (board[1,2]==board[2,2]&board[2,2]==board[3,2]&board[3,2]==symbol) {
          cat("player",symbol,"win")
          
          break
        }else if (board[1,3]==board[2,3]&board[2,3]==board[3,3]&board[3,3]==symbol) {
          cat("player",symbol,"win")
          break
        } else if(board[1,1]==board[2,2]&board[2,2]==board[3,3]&board[3,3]==symbol){
          cat("player",symbol,"win")
          break
        } else if (board[1,3]==board[2,2]&board[2,2]==board[3,1]&board[3,1]==symbol) {
          cat("player",symbol,"win")
          break
          
          ## to test if there is a TIE      
          
        }else if (w==0) {
          cat("TIE")
          break
          
        }
        
        
        ## player o turn    
        
        print("..................................",quote=F)
        
        
        print("player o turn :",quote=F)
        Sys.sleep(2)
        q <-  which(board == "NaN")
        choice <- sample(x=q,size = 1)
        board[choice] <- "o"
        e <<- which(board == "NaN")
        
        w<- length(e)
        print("current board :",quote=F)
        print("..................................",quote=F)
        Sys.sleep(1)
        print(board)
        
        
        #### if user choose n    
        
      } else{
        
        while (confirmation=="n") {
          
          
          input_row()
          
          
          
          input_col()
          
          
          
          cat("Are you sure you want to put it on row ",row_t)
          cat(" and col ",col_t)
          cat(" ?")
          cat("[ y/n]")
          
          input_confirm()
          
        }
        
        board[row_t,col_t] <- "x"
        print(board)
        
        
        
        
        
        print("player o turn")
        
        q<-  which(board == "NaN")
        choice <- sample(x=q,size = 1)
        board[choice] <- "o"
        e <<-which(board == "NaN")
        
        w<- length(e)
        Sys.sleep(1)
        print(board)
        
        
        
        
        
      }
      
      
      
      ## test if o win
      
      
      if (board[1,1]==board[1,2]&board[1,2]==board[1,3]&board[1,3]=="o"){
        cat("player o win")
        
        break
      } else if (board[2,1]==board[2,2]&board[2,2]==board[2,3]&board[2,3]=="o") {
        cat("player o win")
        break
      } else if(board[3,1]==board[3,2]&board[3,2]==board[3,3]&board[3,3]=="O"){
        cat("player o win")
        
        break
      } else if(board[1,1]==board[2,1]&board[2,1]==board[3,1]&board[3,1]=="o"){
        cat("player o win")
        break
      }else if (board[1,2]==board[2,2]&board[2,2]==board[3,2]&board[3,2]=="o") {
        cat("player o win")
        
        break
      }else if (board[1,3]==board[2,3]&board[2,3]==board[3,3]&board[3,3]=="o") {
        cat("player o win")
        break
      } else if(board[1,1]==board[2,2]&board[2,2]==board[3,3]&board[3,3]=="o"){
        cat("player o win")
        break
      } else if (board[1,3]==board[2,2]&board[2,2]==board[3,1]&board[3,1]=="o") {
        cat("player o win")
        break
      } 
      
      
      
    }
    break
    
  }
  
  ######if player choose o
  
} else if (symbol=="o"){
  repeat{
    for (i in v) {
      print(paste("#####################################",i,"#######################################"),quote = F)#,justify = "left",right=T)
      
      
      Sys.sleep(1)
      print("current board :",quote=F)
      cat("......................................................",fill =T)
      Sys.sleep(1)
      print(board)
      Sys.sleep(2)
      print( "palyer's x turn:",quote=F)
      Sys.sleep(1)
      
      ### player x 'computer' turn      
      
      q <-  which(board == "NaN")
      choice <- sample(x=q,size = 1)
      board[choice] <- "x"
      e <<-which(board == "NaN")
      
      w <<- length(e)
      
      print("current board :",quote=F)
      print("..................................",quote=F)
      Sys.sleep(2)
      print(board)
      
      ## to test if x win      
      
      if (board[1,1]==board[1,2]&board[1,2]==board[1,3]&board[1,3]=="x"){
        cat("player x win")
        
        break    
      } else if (board[2,1]==board[2,2]&board[2,2]==board[2,3]&board[2,3]=="x") {
        cat("player x win")
        break
      } else if(board[3,1]==board[3,2]&board[3,2]==board[3,3]&board[3,3]=="x"){
        cat("player x win")
        
        break
      } else if(board[1,1]==board[2,1]&board[2,1]==board[3,1]&board[3,1]=="x"){
        cat("player x win")
        break
      }else if (board[1,2]==board[2,2]&board[2,2]==board[3,2]&board[3,2]=="x") {
        cat("player x win")
        
        break    
      }else if (board[1,3]==board[2,3]&board[2,3]==board[3,3]&board[3,3]=="x") {
        cat("player x win")
        break 
      } else if(board[1,1]==board[2,2]&board[2,2]==board[3,3]&board[3,3]=="x"){
        cat("player x win")
        break 
      } else if (board[1,3]==board[2,2]&board[2,2]==board[3,1]&board[3,1]=="x") {
        cat("player x win")
        break 
        
        ## test if there is a TIE                
        
      } else if (w==0) {
        cat("TIE")
        break
      }
      
      
      
      
      
      ##### player o turn      
      
      
      print("..................................",quote=F)
      Sys.sleep(1)
      print( "palyer's o turn:",quote=F)
      
      
      ## get row      
      
      input_row()
      
      ## get col            
      
      input_col()
      
      ## to make sure that o put in empty square  
      
      while (board[row_t,col_t]!="NaN") {
        Sys.sleep(1)
        print("Invalid! this place is already taken")
        
        input_row()
        
        input_col()
      }
      
      ## to get y or n      
      
      cat("Are you sure you want to put it on row ",row_t)
      cat(" and col ",col_t)
      cat(" ?")
      cat("[ y/n]")
      Sys.sleep(1)
      
      input_confirm()
      
      if(confirmation=="y"){
        board[row_t,col_t]<- "o"
        print("current board :",quote=F)
        cat("......................................................",fill =T)
        Sys.sleep(1)
        print(board)
        
        
      } else{
        
        while (confirmation=="n") {
          
          
          input_row()
          
          input_col()
          
          
          
          cat("Are you sure you want to put it on row ",row_t)
          cat(" and col ",col_t)
          cat(" ?")
          cat("[ y/n]")
          
          input_confirm()
          
        }
      }
      board[row_t,col_t] <- "o"
      print("current board :",quote=F)
      cat("......................................................",fill =T)
      Sys.sleep(1)
      print(board)
      
      
      
      
      ### to test if o win
      
      if (board[1,1]==board[1,2]&board[1,2]==board[1,3]&board[1,3]==symbol){
        cat("player",symbol,"win")
        
        break    
      } else if (board[2,1]==board[2,2]&board[2,2]==board[2,3]&board[2,3]==symbol) {
        cat("player",symbol,"win")
        break
      } else if(board[3,1]==board[3,2]&board[3,2]==board[3,3]&board[3,3]==symbol){
        cat("player",symbol,"win")
        
        break
      } else if(board[1,1]==board[2,1]&board[2,1]==board[3,1]&board[3,1]==symbol){
        cat("player",symbol,"win")
        break
      }else if (board[1,2]==board[2,2]&board[2,2]==board[3,2]&board[3,2]==symbol) {
        cat("player",symbol,"win")
        
        break    
      }else if (board[1,3]==board[2,3]&board[2,3]==board[3,3]&board[3,3]==symbol) {
        cat("player",symbol,"win")
        break 
      } else if(board[1,1]==board[2,2]&board[2,2]==board[3,3]&board[3,3]==symbol){
        cat("player",symbol,"win")
        break 
      } else if (board[1,3]==board[2,2]&board[2,2]==board[3,1]&board[3,1]==symbol) {
        cat("player",symbol,"win")
        break 
      } 
      
    }
    break
    
  }
  
}