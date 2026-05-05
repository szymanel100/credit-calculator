# credit-calculator

Set in VS Code :
ctr+shift+p --> "Preferences: Open Workspace Settings (JSON)"

settings.json

{
    "terminal.integrated.env.windows": {
        "PATH": "${env:PATH};C:\\gnucobol\\bin"
    }
}




Command to run in CMD:
1) C:\gnucobol\bin>
cobc -conf "C:\gnucobol\config\default.conf" -x "C:\Users\Szymon\cobol\calculator_credit\credit-calculator\credit_calculator.cbl"

2) .\credit_calculator.exe
