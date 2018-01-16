# En løsning til coding game - War (Queues)

En implementation af spillet Krig.

Hver spiller har en bunke kort og vender samtidigt et kort af gangen fra toppen af deres bunke 
Den spiller der har det højeste kort lægger først spiller 1's kort i bunden af sin bunde og derefter spiller 2's kort i bunden  
Hvis kortene er lige høje lægges yderligere 4 kort ned i hver spillers krigsbunke og den der nu har det højeste lægger først spiller 1s krigsbunke i bunden af sin bunke og dernæst spiller 2. 
Man starter med at lægge det nederste kort i krigsbunken os så fremdeles.
Hvis kortene efter krig stadig er ligestore gentages krig indtil der er et resultat.
Et vilkårligt antal krige tæller kun som en spille runde.
Hvis en spiller løber tør for kort under en krig (altså inden det sidste kort i krigen er spillet) er det uafgjort.
Ellers vinder den spiller der til sidst har vundet alle kortene.
Målet er givent et input at angive hvilken spiller der vinder og i hvilken runde eller at angive PAT
    
Link til problemet https://www.codingame.com/training/medium/winamax-battle

