-- Варіант 10. Розв’язати нелінійне рівняння x=cos(x) методами перебору та хорд, 
-- визначивши інтервал [a, b], на якому існує рішення рівняння. 
-- Порівняти результати розв’язків двома методами. 

x0 = 0.0
x1 = 1.0
ee = 0.001

torun a b e = foo a b e 0 cos

foo xprev xcurr e xnext f | (abs tmp - xnext) > e = foo xcurr xnext e tmp f
  | otherwise = xnext
  where tmp = xcurr - (f xcurr) * (xprev - xcurr) / ((f xprev) - (f xcurr))