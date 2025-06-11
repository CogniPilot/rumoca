Run Docker with port mapping (This allows jupyter notebook to run)
sudo docker run -p 8888:8888 -it rumoca-container

## Run Rumoca:
Example 1: 

rumoca --help

Example 2 mass-spring-damper model to casadi:

rumoca models/msd.mo -t templates/casadi_dae.jinja


Example 3: save output to file:

rumoca models/msd.mo -t templates/casadi_dae.jinja > msd_casadi_example.py



## Run example notebooks to explore exported python models:
jupyter lab --ip=0.0.0.0 --port=8888 --allow-root --NotebookApp.token=''

Launch browser and go to: http://localhost:8888

Explore jupyter notebooks.
