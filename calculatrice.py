nombre1=float(input('premier nombre >:'))
action=input('quel action voulez vous faire ? + - / *:')
nombre2=float(input('deuxieme nombre >:'))
if action == '+':
    print("le resultat est",nombre1+nombre2)
elif action == '*':
    print("le resultat est",nombre1*nombre2)
elif action == '/':
    print("le resultat est",nombre1/nombre2)
elif action == '-':
    print("le resultat est",nombre1-nombre2)
else:
   print("l'action n'est pas valide")  
