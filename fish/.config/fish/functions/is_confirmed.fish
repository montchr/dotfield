function is_confirmed -d "Test whether the result of an 'ask' is a confirmation"
    if "$REPLY" =~ ^[Yy]$
      return 0
    end
    return 1
end
