function tre --description '`tre` is a shorthand for `tree` with some helpful adjustments'
	tree -aC -I '.git|node_modules' --dirsfirst $argv | less -FRNX;
end
