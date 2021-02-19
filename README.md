# shiny-monarch-diseases

## Overview
This is a simple Shiny application that extract phenotypic features from a FHIR server and request potential disease matches from the Monarch API. There are several versions demonstrating a couple of authentication types.

## app-cookie
This workflow can be used with the 2020 Kids First development server. It requires a cookie from an out of band login.

## app-googleauthr
This workflow uses Google's OAuth2.0 implementation and the googleAuthR pacakge to generate credentials and interact with a GCP FHIR store. One will need to create an OAuth 2.0 Client ID here: https://console.developers.google.com/apis/credentials of type "Web application". I had success with JavaScript origins and redirect URIs of:

* http://127.0.0.1:1221
* http://localhost:1221

I'm not sure that both are necessary. After creating it, hit the download button on the credentials page for the new web app, and then move it to the Shiny app directory as "web_secret.json". On startup, it will run you through the a Google OAuth flow. In theory, it will retry failures and attempt to refresh the bearer token, but it may fail nonetheless.