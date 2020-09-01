# http://askubuntu.com/a/48611
function fndupes -d "Find all filename duplicates in subdirectories"
  find . -type f -exec basename {} \; | sed 's/\(.*\)\..*/\1/' | sort | uniq -c | grep -v "^[ \t]*1 "
end
