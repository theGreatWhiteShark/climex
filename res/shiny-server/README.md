This is a short guide to install and run the climex server on your computer. Since we will use shiny-server it is not just accessible via localhost but throughout your entire network.

## Prerequisites

For this tutorial it is expected you already followed all the steps in the [installation guide](https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source) of the [shiny-server](https://www.rstudio.com/products/shiny/shiny-server/).

In addition you have to have the **climex** package install on your system.

```{bash}
## Switching to the superuse
sudo su

## Starting R (if you have your own compiled version, make sure its linked against /usr/local/bin/R)
R --no-init-file
```

```{R}
## Inside R get the newest version of the devtools package
install.packages( "devtools" )

## Installing the newest climex version on the master branch
devtools::install_github( "theGreatWhiteShark/climex" )
```

## Configuration

In order to run the server you have to copy all its configuration files and scripts into shiny-server's directories

```{bash}
## Climex server configuration files and resources
sudo cp --recursive climex/ assets/ /srv/shiny-server/

## Configuration of the local R environment of the shiny user needed start up the shiny-server
sudo cp .Rprofile /home/shiny/.Rprofile

## Change the owner of all those files to the shiny user
sudo chown shiny /home/shiny/.Rprofile /srv/shiny-server/*
```

Now we need the station data of the German weather service to supply the time series used within the climex server.

```{bash}
## Change to the shiny user 
sudo su shiny

## Start an R process to download the data
R --no-init-file
```

```{R}
## Load the climex package
require( climex )

## Set the climex home folder to the server's assets folder (since we omitted the .Rprofile file)
options( climex.path = "/srv/shiny-server/assets/" )

## Download the DWD data into the asset. This will take a while.
download.data.dwd()
```
