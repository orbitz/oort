Nonterminals expr.

Terminals 'string' 'on' 'is' 'factoid' 'reply' '>' '!'.

Rootsymbol expr.

expr -> factoid 'is' string : {factoid_set, '$1', 'is', '$3'}.
expr -> factoid 'is' 'reply' string : {factoid_set, '$1', 'reply', '$4'}.
expr -> factoid string : {factoid_read, '$1', '$2'}.
expr -> factoid '>' factoid string : {factoid_read_who, '$1', '$3', '$4'}.
expr -> factoid '!' factoid string : {factoid_read_who, '$1', '$3', '$4'}.
expr -> factoid : {factoid_read, '$1', nil}.
