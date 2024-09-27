

# GCAMUSAJobs

<br />

<!-------------------------->
<!-------------------------->
# <a name="Contents"></a>Contents
<!-------------------------->
<!-------------------------->

- [Key Links](#KeyLinks)
- [Introduction](#Introduction)
- [Citation](#Citation)
- [Installation Guide](#InstallGuides)
- [How-to Guides](#How-toGuides)
- [User Notice](#UserNotice)

<br />

<!-------------------------->
<!-------------------------->
# <a name="KeyLinks"></a>Key Links
<!-------------------------->
<!-------------------------->

- Github: https://github.com/JGCRI/GCAMUSAJobs/tree/paper
- Webpage: https://jgcri.github.io/GCAMUSAJobs/

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="Introduction"></a>Introduction
<!-------------------------->
<!-------------------------->

`GCAMUSAJobs` was developed to post-process electric power projections from GCAM-USA, enabling the estimation of future power sector jobs at the state-level by generation technology and job type. GCAMUSAJobs extends GCAM-USA functionality by (1) estimating the capacity levels of different activities – operational capacity, capacity addition, and retirement; and (2) calculating jobs associated with production activities, including those in operation and maintenance (O&M), construction, and decommissioning.

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="Citation"></a>Citation
<!-------------------------->
<!-------------------------->


[Back to Contents](#Contents)

<br />


<!-------------------------->
<!-------------------------->
# <a name="InstallationGuides"></a>Installation Guides
<!-------------------------->
<!-------------------------->

1. Download and install:

    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)

2. For Linux users, install following libraries:

```
sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
sudo apt-get install libxml2-dev
```
    
3. Open R studio:

```
install.packages('devtools')
devtools::install_github('JGCRI/rgcam')
devtools::install_github('JGCRI/GCAMUSAJobs')
```

[Back to Contents](#Contents)

<br />


<!-------------------------->
<!-------------------------->
# <a name="How-toGuides"></a>How-to Guides
<!-------------------------->
<!-------------------------->

- [Package vignette](https://jgcri.github.io/GCAMUSAJobs/articles/package_vignette.html)

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="UserNotice"></a>User Notice
<!-------------------------->
<!-------------------------->

`GCAMUSAJobs`, by default, works with GCAM-USA v7.1 , and is compatible with outcomes with GCAM v6 or later versions conditional updated GCAM-USA assumption input. `GCAMUSAJobs` provides annual average job estimates during the 5-year window of a model timestep.


[Back to Contents](#Contents)

<br />
