# Trata de personas y acceso a la salud en México: análisis de econometría espacial (2016–2022)

Datos, código y resultados del artículo **"La ausencia del goce efectivo del derecho a la salud y su impacto en la incidencia delictiva de la trata de personas en México 2016–2022"**, presentado en la XV Reunión Nacional de Investigación Demográfica (El Colegio de México, 2025) y en el INEGI.

## Objetivo

Analizar si las fallas en el acceso al sistema de salud, junto con otros factores de vulnerabilidad, se asocian con la incidencia de la trata de personas en las entidades federativas de México, y si este fenómeno presenta dependencia espacial entre entidades vecinas (énfasis en 2022, año posterior a la pandemia).

## Fuentes de información

- **ENIGH** (INEGI): indicadores de gasto de los hogares en salud y acceso efectivo al sistema de salud.
- **Sistema de Cuentas Nacionales** (INEGI, año base 2018): PIB por entidad federativa.
- **ENVIPE** (INEGI): victimización y percepción de seguridad.
- **Carpetas de investigación del SESNSP**: incidencia delictiva de trata de personas.

## Metodología

1. **Construcción de indicadores en Stata** a partir de las encuestas y registros oficiales, y consolidación de una base por entidad federativa.
2. **Base geoespacial en QGIS** con los indicadores construidos.
3. Estimación de un modelo lineal **MCO** como punto de partida y diagnóstico de supuestos: normalidad (Jarque-Bera, Shapiro-Wilk) y heterocedasticidad (Breusch-Pagan).
4. Pruebas de autocorrelación espacial en los residuos: **LM-Lag, LM-Error y sus versiones robustas** (Anselin, 1988; Anselin et al., 1996).
5. Comparación de modelos espaciales en **R**: **SAR** (rezago espacial), **SEM** (error espacial) y **Durbin espacial (SDM)**.
6. Selección del **modelo SAR** según las pruebas LM y cálculo de **efectos directos, indirectos y totales** (derrame espacial).

## Principales resultados

- El coeficiente espacial del modelo SAR es significativo y negativo (**ρ = −0,52**): la trata se **concentra** en focos territoriales específicos, en lugar de dispersarse de forma homogénea.
- La **informalidad laboral** y la **movilidad de la población** tienen efectos totales positivos y significativos: la vulnerabilidad se **transmite regionalmente** hacia las entidades vecinas.
- Las deficiencias del sistema de salud no son una causa directa, pero el **gasto catastrófico en salud** actúa como catalizador al impulsar desplazamientos y empleos informales.
- Tras estimar el SAR no queda autocorrelación espacial en los residuos.

## Contenido del repositorio

| Archivo | Descripción |
|---|---|
| `salud 3.0.do`, `salud 4.0.do` | Código en Stata para el diseño y cálculo de los indicadores (ENIGH, PIB, ENVIPE, SESNSP) |
| `PP.0 CON IGUAL A 3.R` | Código en R de los modelos de econometría espacial (MCO, SAR, SEM, SDM) |
| `BASE VF.xlsx` | Base de datos final con el procedimiento y el cálculo de los indicadores |
| `Bse 13.xlsx` | Base para los cálculos temporales de los indicadores construidos |
| `BASE 11.shp` | Capa geoespacial por entidad federativa elaborada a partir de `BASE VF` |
| `Resultados vf (1).pdf` | Resultados de los modelos, análisis y cartografía generada |

## Herramientas

Stata · QGIS · R (econometría espacial) · Excel

## Autora

**Luisa Fernanda Trujillo Merchán** – Economista, Maestría en Economía Urbana y Regional (UNAM)
[LinkedIn](https://www.linkedin.com/in/luisa-trujillo/)
