#Proyecto CaMa


Proyecto CaMa es un estudio de asociación entre de dos variantes en el gen ESR1 y una región intergénic.a
El genotipaje se realizó principalmente  mediante la técnica de HRM y Secuenciamiento Sanger (2018) tanto en un grupo pacientes, como en un grupo control . 
La data fue recolectada es una tabla excel, los scripts presentes está diseñados para trabajar desde tablas excel. 


#SNPS

Región intergénica: Rs614367 (Alelo wt: C
			      Alelo mt: T)

ESR1: rs140068132 (alelo wt: A
		   alelo mt: G)






#Análisis

Los análisis estadísticos a realizar son el cálculo del OR crudo y OR ajustado
La variables para el análisis del OR crudo son las siguientes, tanto para el SNP en ESR1 y el SNP en la región intergénica: 




				|Presencia_mutación  |   Ausencia_de_mutación
		________________|____________________|_________________________
		grupo_pacientes |                    |
		________________|____________________|_________________________
				|                    |
		grupo_control   |		     |






#Scripts 

El órden de los scrips para realizar el análisis es el siguiente:

	1) Proyecto CaMa.2.R
	2) ORs CaMa.R
	3)

#Resultados 

La asociación que sí tiene significado estadístico es el OR para el SNP de la región intergénica
(OR crudo = 0.41 (0.20, 0.82) p=0.010), como factor riesgo.


ESR1 OR = 0.80 (0.61, 1.25) 



