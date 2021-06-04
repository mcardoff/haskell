1.36 + -0.0235x + 1.64E-04x^2 + -5.49E-07x^3 + 9.5E-10x^4 + -7.82E-13x^5 + 2.39E-16x^6

toPython :: String -> String
toPython [] = []
toPython (x:xs) = case x of
                    'x' -> " * (wl **" ++
