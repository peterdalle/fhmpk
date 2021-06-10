# Journalisternas frågor vid Folkhälsomyndighetens presskonferens 2020

Kvantitativ innehållsanalys av journalisters frågor vid Folkhälsomyndighetens presskonferens 2020.

Resultatet finns i bokkapitlet [Ställde journalisterna kritiska frågor under Folkhälsomyndighetens presskonferenser? (PDF)](https://mediestudier.se/wp-content/uploads/2021/03/Stallde-journalisterna-kritiska-fragor-under-FHMs-presskonferenser-1.pdf) som finns i boken [Journalistik i coronans tid](https://mediestudier.se/publikationer/journalistik-i-coronans-tid/) utgiven av Institutet för mediestudier.

## Analys

Analysfiler finns i katalogen R.

## Metod

- Urval: Obundet slumpmässigt urval av alla de dagar som Folkhälsomyndighetens höll presskonferenser under 2020.
- Data: Folkhälsomyndighetens presskonferenser samt personliga intervjuer strax efter presskonferenserna i SVT och Aftonbladet

## Data och kodbok

[**Ladda ned datafil med innehållsanalys (SPSS-fil)**](https://raw.githubusercontent.com/peterdalle/fhmpk/main/Data/covid-210201_1.sav)

Datafilen är kodad av Kalle Färm (singelkodad, utan reliabilitetsanalys) och
inbegriper förutom en klassificering av turordningar/frågor (n = 1 215) även själva
frågorna från både Folkhälsomyndighetens presskonferens, SVT samt Aftonbladet.

Kodschema finns i [appendix till bokkapitlet (PDF)](https://mediestudier.se/wp-content/uploads/2021/03/Stallde-journalisterna-kritiska-fragor-under-FHMs-presskonferenser-1.pdf#page=18). 

### Variabler i datafilen

```
# summarytools::dfSummary(df)

Data Frame Summary 
Dimensions: 1215 x 35  
Duplicates: 0  

-----------------------------------------------------------------------------------------------------------------------------------
No   Variable             Stats / Values                    Freqs (% of Valid)      Graph                      Valid      Missing  
---- -------------------- --------------------------------- ----------------------- -------------------------- ---------- ---------
1    Datum                1. 2020-04-14                      53 ( 4.4%)                                        1215       0        
     [character]          2. 2020-03-11                      48 ( 4.0%)                                        (100.0%)   (0.0%)   
                          3. 2020-03-18                      46 ( 3.8%)                                                            
                          4. 2020-05-06                      46 ( 3.8%)                                                            
                          5. 2020-06-05                      45 ( 3.7%)                                                            
                          6. 2020-10-06                      44 ( 3.6%)                                                            
                          7. 2020-11-12                      44 ( 3.6%)                                                            
                          8. 2020-04-16                      43 ( 3.5%)                                                            
                          9. 2020-11-17                      43 ( 3.5%)                                                            
                          10. 2020-03-26                     42 ( 3.5%)                                                            
                          [ 27 others ]                     761 (62.6%)             IIIIIIIIIIII                                   

2    Medium               1. FHM presskonferens             856 (70.5%)             IIIIIIIIIIIIII             1215       0        
     [factor]             2. SVT intervju                   235 (19.3%)             III                        (100.0%)   (0.0%)   
                          3. AB intervju                    124 (10.2%)             II                                             

3    Tidskod              Mean (sd) : 27.7 (13.6)           914 distinct values           : : .                1215       0        
     [numeric]            min < med < max:                                                : : :                (100.0%)   (0.0%)   
                          0 < 28.1 < 62.6                                           .   . : : : :                                  
                          IQR (CV) : 17.6 (0.5)                                     :   : : : : : .                                
                                                                                    : : : : : : : : : .                            

4    IntervjuPlats        1. Fysisk                         933 (76.8%)             IIIIIIIIIIIIIII            1215       0        
     [factor]             2. Video                          270 (22.2%)             IIII                       (100.0%)   (0.0%)   
                          3. Audio                           12 ( 1.0%)                                                            

5    Intervjuare          1. AB                             193 (15.9%)             III                        1215       0        
     [factor]             2. DN                              97 ( 8.0%)             I                          (100.0%)   (0.0%)   
                          3. EXP                             27 ( 2.2%)                                                            
                          4. GP                              55 ( 4.5%)                                                            
                          5. Nyheter Idag                     8 ( 0.7%)                                                            
                          6. SvD                             27 ( 2.2%)                                                            
                          7. SR                              54 ( 4.4%)                                                            
                          8. Ekot                            58 ( 4.8%)                                                            
                          9. Vetenskapsradion                31 ( 2.6%)                                                            
                          10. TV4                            44 ( 3.6%)                                                            
                          [ 9 others ]                      621 (51.1%)             IIIIIIIIII                                     

6    Respondent           1. FHM: Tegnell                   826 (68.0%)             IIIIIIIIIIIII              1215       0        
     [factor]             2. FHM: Tegmark Wisell             94 ( 7.7%)             I                          (100.0%)   (0.0%)   
                          3. FHM: Wallensten                 49 ( 4.0%)                                                            
                          4. FHM: Carlsson                   36 ( 3.0%)                                                            
                          5. FHM: Giesecke                    0 ( 0.0%)                                                            
                          6. FHM: Annan                      65 ( 5.3%)             I                                              
                          7. Socialstyrelsen: Alexande       13 ( 1.1%)                                                            
                          8. Socialstyrelsen: Sandwall       43 ( 3.5%)                                                            
                          9. Socialstyrelsen: Annan          42 ( 3.5%)                                                            
                          10. MSB                            29 ( 2.4%)                                                            
                          [ 4 others ]                       18 ( 1.5%)                                                            

7    Turordning           Mean (sd) : 4.6 (3.9)             19 distinct values      :                          1215       0        
     [numeric]            min < med < max:                                          :                          (100.0%)   (0.0%)   
                          1 < 3 < 19                                                :                                              
                          IQR (CV) : 6 (0.9)                                        : . . .                                        
                                                                                    : : : : : . .                                  

8    Frågenummer          Mean (sd) : 2.6 (1.9)             14 distinct values      :                          1215       0        
     [numeric]            min < med < max:                                          :                          (100.0%)   (0.0%)   
                          1 < 2 < 14                                                :                                              
                          IQR (CV) : 2 (0.8)                                        :                                              
                                                                                    : : . .                                        

9    Uppföljningsfråga    1. Nej                            622 (51.2%)             IIIIIIIIII                 1215       0        
     [factor]             2. Ja                             593 (48.8%)             IIIIIIIII                  (100.0%)   (0.0%)   
                          3. N/A                              0 ( 0.0%)                                                            

10   Komplexitet          1. Låg                            740 (60.9%)             IIIIIIIIIIII               1215       0        
     [factor]             2. Hög                            460 (37.9%)             IIIIIII                    (100.0%)   (0.0%)   
                          3. Blandad                         14 ( 1.2%)                                                            
                          4. Ej relevant                      0 ( 0.0%)                                                            
                          5. N/A                              1 ( 0.1%)                                                            

11   Ämne                 1. Covid-19                        37 ( 3.0%)                                        1215       0        
     [factor]             2. Smittspridning                  84 ( 6.9%)             I                          (100.0%)   (0.0%)   
                          3. Statistik, antal fall/död      111 ( 9.1%)             I                                              
                          4. Grupp: Patienter                10 ( 0.8%)                                                            
                          5. Grupp: Äldre                    39 ( 3.2%)                                                            
                          6. Grupp: Yngre                     6 ( 0.5%)                                                            
                          7. Grupp: Barn                      2 ( 0.2%)                                                            
                          8. Grupp: Män                       1 ( 0.1%)                                                            
                          9. Grupp: Kvinnor                   3 ( 0.2%)                                                            
                          10. Grupp: Invandrare               4 ( 0.3%)                                                            
                          [ 40 others ]                     918 (75.6%)             IIIIIIIIIIIIIII                                

12   Självreferens        1. Ingen                          1101 (90.6%)            IIIIIIIIIIIIIIIIII         1215       0        
     [factor]             2. Undran                          103 ( 8.5%)            I                          (100.0%)   (0.0%)   
                          3. Vilja                             7 ( 0.6%)                                                           
                          4. Får/kan                           4 ( 0.3%)                                                           

13   Rättframhet          1. Ingen                          1138 (93.7%)            IIIIIIIIIIIIIIIIII         1215       0        
     [factor]             2. Förmåga                          70 ( 5.8%)            I                          (100.0%)   (0.0%)   
                          3. Vilja                             7 ( 0.6%)                                                           

14   Rådsökande           1. Nej                            1082 (89.1%)            IIIIIIIIIIIIIIIII          1215       0        
     [factor]             2. Ja                              133 (10.9%)            II                         (100.0%)   (0.0%)   

15   Spekulerande         1. Nej                            1018 (83.8%)            IIIIIIIIIIIIIIII           1215       0        
     [factor]             2. Ja                              197 (16.2%)            III                        (100.0%)   (0.0%)   

16   Upprepning           1. Nej                            1157 (95.2%)            IIIIIIIIIIIIIIIIIII        1215       0        
     [factor]             2. Ja                               58 ( 4.8%)                                       (100.0%)   (0.0%)   
                          3. Oklart                            0 ( 0.0%)                                                           

17   Personlig            1. Nej                            1091 (89.8%)            IIIIIIIIIIIIIIIII          1215       0        
     [factor]             2. Ja                              124 (10.2%)            II                         (100.0%)   (0.0%)   
                          3. Oklart                            0 ( 0.0%)                                                           

18   TredjePart           1. Nej                            871 (71.7%)             IIIIIIIIIIIIII             1215       0        
     [factor]             2. Ja                             344 (28.3%)             IIIII                      (100.0%)   (0.0%)   

19   NegativForm          1. Nej                            955 (78.6%)             IIIIIIIIIIIIIII            1215       0        
     [factor]             2. Ja                             260 (21.4%)             IIII                       (100.0%)   (0.0%)   

20   Men                  1. Nej                            1047 (86.2%)            IIIIIIIIIIIIIIIII          1215       0        
     [factor]             2. Ja                              168 (13.8%)            II                         (100.0%)   (0.0%)   

21   JaNej                1. Nej                            592 (48.7%)             IIIIIIIII                  1215       0        
     [factor]             2. Ja                             622 (51.2%)             IIIIIIIIII                 (100.0%)   (0.0%)   
                          3. Oklart                           1 ( 0.1%)                                                            

22   Ansvarsutkrävande    1. Nej                            1017 (83.7%)            IIIIIIIIIIIIIIII           1215       0        
     [factor]             2. Varför                           84 ( 6.9%)            I                          (100.0%)   (0.0%)   
                          3. Hur                              91 ( 7.5%)            I                                              
                          4. Kombination                      22 ( 1.8%)                                                           
                          5. Oklart                            1 ( 0.1%)                                                           

23   Fientlig             1. Nej                            952 (78.4%)             IIIIIIIIIIIIIII            1215       0        
     [factor]             2. Söker svar                     205 (16.9%)             III                        (100.0%)   (0.0%)   
                          3. Inledning korrekt               56 ( 4.6%)                                                            
                          4. Kombination                      1 ( 0.1%)                                                            
                          5. Vet ej                           1 ( 0.1%)                                                            

24   Ton                  1. Negativ                        219 (18.0%)             III                        1215       0        
     [factor]             2. Neutral                        989 (81.4%)             IIIIIIIIIIIIIIII           (100.0%)   (0.0%)   
                          3. Positiv                          7 ( 0.6%)                                                            
                          4. N/A                              0 ( 0.0%)                                                            

25   PratarIMun           1. Nej                            1104 (90.9%)            IIIIIIIIIIIIIIIIII         1215       0        
     [factor]             2. Ja                              111 ( 9.1%)            I                          (100.0%)   (0.0%)   
                          3. N/A                               0 ( 0.0%)                                                           

26   Transkribering       1. ...att inte resa ut i övr         1 ( 0.1%)                                       1215       0        
     [character]          2. [Studien] såg på 3 300 tr         1 ( 0.1%)                                       (100.0%)   (0.0%)   
                          3. …alltså, längre än?               1 ( 0.1%)                                                           
                          4. 6% av de tillfrisknade co         1 ( 0.1%)                                                           
                          5. 70-plussare får också res         1 ( 0.1%)                                                           
                          6. 70-plussare har vi fått v         1 ( 0.1%)                                                           
                          7. A couple of questions on          1 ( 0.1%)                                                           
                          8. Allmänt liksom, har de pr         1 ( 0.1%)                                                           
                          9. Alltså, finns det några l         1 ( 0.1%)                                                           
                          10. Alltså, har ni räknat med        1 ( 0.1%)                                                           
                          [ 1205 others ]                   1205 (99.2%)            IIIIIIIIIIIIIIIIIII                            
-----------------------------------------------------------------------------------------------------------------------------------
```
