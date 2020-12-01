toPart recipient = "Hello " ++ recipient ++ ",\n"
bodyPart job = "Thanks for applying for the " ++ job ++ ".\n"
fromPart author = "Thanks, \n" ++ author
createEmail recipient job author = toPart recipient ++
                                         bodyPart job ++
                                         fromPart author



messyMain :: IO ()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Job?"
    job<- getLine
    print "Who is the author?"
    author <- getLine
    print (createEmail recipient job author)