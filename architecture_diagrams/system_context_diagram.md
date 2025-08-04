# System context diagram

This diagram presents the ecosystem simulation toolkit at the center, surrounded by its users and the external systems 
it interacts with. The emphasis is on the people and the toolkit itself, rather than on technologies or low-level 
implementation details.

```mermaid
%%{init: { 'themeVariables': { 'clusterBkg': 'transparent' } } }%%
flowchart TB

%% Define styles for nodes
classDef borderless stroke-width:0px
classDef lightblue fill:#BBDEFB, color:#000
classDef lightgreen fill:#C8E6C9, color:#000
classDef lightpurple fill:#E1BEE7, color:#000

%% Set up legend for nodes
subgraph Legend
    direction LR
    Legend1[Person]:::lightblue
    Legend2[Toolkit Component]:::lightgreen
    Legend3[External System]:::lightpurple
end

%% Define users
subgraph "" 
    direction LR
    U1[<b>User 1</b><br/><br/>-<i>Person</i>-<br/><br/>Ecosystem Modeler]:::lightblue
    U2[<b>User 2</b><br/><br/>-<i>Person</i>-<br/><br/>General Data User]:::lightblue
    U3[<b>User 3</b><br/><br/>-<i>Person</i>-<br/><br/>Stock Assessment Modeler]:::lightblue
end

%% Define ecosystem simulation toolkit
subgraph "" 
    direction LR
    EcosystemData[<b>ecosystemdata</b><br/><br/> -<i>R package</i>-<br/><br/> -Standardizes model output<br/>-Provides functions to connect with external tools to perform time series analysis]:::lightgreen
    EcosystemOM[<b>ecosystemom</b><br/><br/> -<i>R package</i>-<br/><br/> Samples observations from ecosystemdata for stock assessment testing]:::lightgreen
end

%% Define external systems
subgraph "" 
    direction TB
    EM[<b>Ecosystem Models</b><br/><br/> -<i>External System</i>-<br/><br/> Simulate ecosystem dynamics and produce raw outputs]:::lightpurple
    DSEM[<b>Dynamic Structural Equation Models</b><br/><br/> -<i>External System</i>-<br/><br/> Fit dynamic structural equation models]:::lightpurple
    SA[<b>Stock Assessment Models</b><br/><br/> -<i>External System</i>-<br/><br/> Analyze fisheries and environmental impacts on fish stocks]:::lightpurple
end

%% Define connections
U1--Runs -->EM
EM--Provides raw outputs to -->U1
U1--Uploads ecosystem model raw outputs to -->EcosystemData
EcosystemData--Provides true data to -->EcosystemOM
U2--Accesses standardized data -->EcosystemData
EcosystemData--Provides true data to -->DSEM 
U3--Accesses standardized data and simulates observations -->EcosystemOM 
EcosystemOM--Provides simulated observations to -->SA
%% TODO: link DSEM to SA from left to right. The code below doesn't work.
%%DSEM--Provides improved assumptions and paramters -->SA

class U1,U2,U3,EcosystemOM,Ecosystemdata,EM,DSEM,SA borderless
linkStyle 0 stroke:#ff8800,stroke-width:3px
linkStyle 1 stroke:#ff8800,stroke-width:3px
linkStyle 2 stroke:#ff8800,stroke-width:3px
linkStyle 3 stroke:#6082B6,stroke-width:3px
linkStyle 4 stroke:#e91e63,stroke-width:3px
linkStyle 5 stroke:#e91e63,stroke-width:3px,stroke-dasharray:5,5
linkStyle 6 stroke:black,stroke-width:3px,stroke-dasharray:5,5
linkStyle 7 stroke:black,stroke-width:3px,stroke-dasharray:5,5
%%linkStyle 8 stroke:#62524F,stroke-width:3px,stroke-dasharray:5,5
```