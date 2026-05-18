# StatOccu <img src="inst/app/www/hexsticker_StatOccu.png" align="right" height="139"/>

**StatOccu** es una plataforma interactiva para modelos de ocupación de especies, parte del ecosistema [StatSuite](https://github.com/ManuelSpinola). Diseñada para enseñanza e investigación en ecología y ciencias de la biodiversidad.

## Módulos disponibles

| Módulo | Descripción | Estado |
|--------|-------------|--------|
| Ocupación clásica | Modelos de ocupación con unmarked | ✅ Disponible |
| Ocupación espacial | Modelos espaciales y multi-especie con spOccupancy | 🔄 Próximamente |
| Marco bayesiano | Modelos bayesianos con nimble/Stan/JAGS | 🔄 Próximamente |

## Instalación

```r
install.packages("remotes")
remotes::install_github("ManuelSpinola/StatOccu")
```

## Uso

```r
library(StatOccu)
StatOccu::run_app()
```

## Autor

**Manuel Spínola**  
ICOMVIS · Universidad Nacional · Costa Rica

## Licencia

MIT
