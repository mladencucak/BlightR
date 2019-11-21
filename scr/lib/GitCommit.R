# Afunction to commit changes from R comand line 
# Usefull when script is running remotely
# https://stackoverflow.com/questions/55432420/how-can-i-commit-changes-to-github-from-within-a-r-script

# Git status.
gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

# Git add.
gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

# Git commit.
# gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
#   cmd = sprintf("git commit -m\"%s\"",msg)
#   system(cmd)
# }

# Git push.
gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

library("git2r")

# Insure you have navigated to a directory with a git repo.
# dir <- "mypath"
# setwd(dir)
# getwd()
# Configure git.
git2r::config(user.name = "mladencucak",user.email = "mladencucak@gmail.com")

# Check git status.
gitstatus()

# Download a file.
url <- "https://i.kym-cdn.com/entries/icons/original/000/002/232/bullet_cat.jpg"
destfile <- "bullet_cat.jpg"
download.file(url,destfile)

# Add and commit changes. 
gitadd()
gitcommit()

# Push changes to github.
gitpush()



