{-
cmd ::= pen mode
| moveto (pos,pos)
| def name ( pars ) cmd
| call name ( vals )
| cmd; cmd
mode ::= up | down
pos ::= num | name
pars ::= name, pars | name
vals ::= num, vals | num
-}

data Cmd = Pen Mode
			| MoveTo Pos Pos
			| Def String Pars Cmd
			| Call String Vals 
			| Double Cmd Cmd

data Mode = Up | Down
data Pos = PosNum Int | PosName String
data Pars = ParsNameAnd String Pars | ParsName String
data Vals = ValsNumAnd Int Vals | ValsNum Int 



vector = Def 	"vector" 
				(ParsNameAnd	"x1" 
								(ParsNameAnd 	"y1" 
												(ParsNameAnd 	"x2" 
																(ParsName "y2")))) 
				(Double (MoveTo (PosName "x1") 
								(PosName "y1")) 
						(Double (Pen Down) 
								(Double (MoveTo (PosName "x2")
												(PosName "y2"))
										(Pen Up))))
				

				
	
		
steps :: Int -> Cmd
steps 0 = Pen Up
steps x = Double 	(MoveTo	(PosNum x) 
							(PosNum x))
					(Double (Pen Down)
							(Double (MoveTo (PosNum (x-1))
											(PosNum x))
									(Double (MoveTo (PosNum (x-1))
													(PosNum (x-1)))
											(steps (x-1)))))
	
	

			

{-			
circuit ::= gates links
gates ::= num:gateFn ; gates | ϵ
gateFn ::= and | or | xor | not
links ::= from num.num to num.num; links | ϵ
-}

data Circuit = Crct Gates Links
data Gates = Gt Int GateFn Gates
			| GatesEnd
data GateFn = And | Or | XOr | Not
data Links = From (Int,Int) (Int, Int) Links 
			| LinksEnd		
			
halfAdder = Crct	(Gt 1 
						XOr 
						(Gt	2
							And
							GatesEnd))
					(From	(1,1)
							(2,1)
							(From	(1,2)
									(2,2)
									LinksEnd))
					
ppCircuit :: Circuit -> IO()
ppCircuit (Crct g l) = putStrLn (ppGates g ++ ppLinks l)

ppGates :: Gates -> String
ppGates GatesEnd = ""
ppGates (Gt i gf g) = show i ++ ":" ++ ppGateFn gf ++ ";\n" ++ ppGates g


ppGateFn :: GateFn -> String
ppGateFn And = "and"
ppGateFn Or = "or"
ppGateFn XOr = "xor"
ppGateFn Not = "not"

ppLinks :: Links -> String
ppLinks LinksEnd = ""
ppLinks (From x y l) = "from " ++ show (fst x) ++ "." ++ show (snd x)
						++ " to " ++ show (fst y) ++ "." ++ show (snd y) 
						++ ";\n" ++ ppLinks l

			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			