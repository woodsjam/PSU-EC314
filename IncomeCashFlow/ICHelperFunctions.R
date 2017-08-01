# Income and Cash Flow Helper Functions
# 

TaxRate <- 

OperatingRevenue <-
OperatingExpenses <-
Interest <-
Depreciation <-
  
AssetChange <-
AssetLable <-

FinanceChange <-
FinanceLabel <-

WorkingCapital <-
GainsTax <-
  
# Calculated   
TaxableIncome <- OperatingRevenue - OperatingExpenses - Interest - Depreciation
Tax <- TaxableIncome * TaxRate
NetIncome <- TaxableIncome - Tax
NetCash <- NetIncome + Depreciation + AssetChange + WorkingCapital - GainsTax + FinanceChange 


PrintTable <- function(accData){

\begin{center}
\begin{table}[!h]
\begin{Large}
\begin{tabular}{l||c|c|c|c}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
  
  %%                                                                  %%
  %%  This is a LaTeX2e table fragment exported from Gnumeric.        %%
  %%                                                                  %%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  \multicolumn{5}{c}{Income Statement}\\
&Year 0  &Year 1  &Year 2  &Year 3\\
Operating Revenue	&	&\Sexpr{SalesInc}	&\Sexpr{SalesInc}	&\Sexpr{SalesInc}\\
Operating Expenses	&	&	&	&\\
Interest	&	&	&	&\\
Depreciation	&\Sexpr{Depreciation[1]}	&\Sexpr{Depreciation[2]}	&\Sexpr{Depreciation[3]}	&\Sexpr{Depreciation[4]}\\
\hline
Taxable Income	&	&	&	&\\
Tax	&	&	&	&\\
\hline
Net Income	&\Sexpr{NetIncome[1]}	&\Sexpr{NetIncome[2] }	&\Sexpr{NetIncome[3] }	& \Sexpr{NetIncome[4] }\\
\\
\multicolumn{5}{c}{Cash Flow Statement}\\
Operations&	&	&	&\\
\hspace{.25in}Net Income	&\Sexpr{NetIncome[1]}	&\Sexpr{NetIncome[2] }	&\Sexpr{NetIncome[3] }	& \Sexpr{NetIncome[4] }\\
\hspace{.25in}Depreciation	&\Sexpr{Depreciation[1]}	&\Sexpr{Depreciation[2]}	&\Sexpr{Depreciation[3]}	&\Sexpr{Depreciation[4]}\\
Investments	&	&	&	&\\
CNC&\Sexpr{-Cost}	&	&	&\$8000\\
\hspace{.25in}Working Capital	&	&-\$5000	&	&\$5000\\
\hspace{.25in}Gains Tax	&	&	&	&\Sexpr{GainsTax}\\
&	&	&	&\\

Finance	&	&	&	&\\
&	&	&	&\\
\hline
Net Cash Flow	&	\Sexpr{NetCash[1]}&\Sexpr{NetCash[2]}	&\Sexpr{NetCash[3]}	&\Sexpr{NetCash[4]}\\
\end{tabular}
\end{Large}
\end{table}
\end{center}
}