# 4. Stwórz funkcję, która oblicza długość przeciwprostokątnej w trójkącie prostokątnym.

przeciwprostokatna = function(a, b) {
  c=sqrt(a^2+b^2)
  return(c)
}

# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.

kostka = function(n) {
  wynik=sample(1:6,n,replace=TRUE)
  return(wynik)
}

