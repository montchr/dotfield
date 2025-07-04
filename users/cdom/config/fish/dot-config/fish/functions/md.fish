function md -d "mkdir with parents, then cd to that directory"
  set -l bold_white "\e[1;37m"
  set -l underline_white "\e[4;37m"
  set -l reset "\e[0m"

  mkdir -p $argv
  cd $argv
  printf "=> $bold_white$underline_white%s$reset" $PWD
end
