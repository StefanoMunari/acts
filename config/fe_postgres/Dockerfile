################################################################################
# Provide an image for the postgres component:
# - configures the DB to accept the incoming AS connections
################################################################################
# base image
FROM postgres:latest

ADD init-user-db.sh /docker-entrypoint-initdb.d/
RUN chmod +x /docker-entrypoint-initdb.d/init-user-db.sh