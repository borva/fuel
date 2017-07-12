
This project is based on the historical fuel price data from 
https://creativecommons.tankerkoenig.de/.

For background on the project see the presentation in the project folder

The code pieces ("A Code XYZ", "B Code XYZ" etc) in this project are 
(C) 2017 Boris Vaillant, Quantitative Consulting 
and are made available to the public under the MIT license

----------------------------------------------
MIT License

Copyright (c) 2017 Boris Vaillant, Quantitative-Consultiung

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

----------------------------------------------

The data for the project (in folders "A DataIn", "B DataIn") is made available under
different public licenses mentioned in the code and in the presentation.
A "non-commercial" restriction applies to the data from "www.gadm.org", which I do not
distribute as part of this project package. The gadm data can however be used for educational purposes likethis workshop.

Please check that the license for downloading this piece is appropriate for your use case.

The structure of the project is as follows:

* "A Code XYZ" reads data from "A DataIn" and writes the result to "Z DataOut"
* "B Code XYZ" reads data from "B DataIn" and (possibly) "Z DataOut" and writes the result to "Z DataOut"
* etc.

There is no input data for steps F + G as these are fully concerned
with representing and visualising the results from the former steps
in "Z DataOut".

In those cases where running the code would take a lot of time or space,
we also provide stored versions of the output in "Z DataOut".
These are recognised by the suffix "_KEEP".
By default, subsequent code pieces refer to these "_KEEP" versions. 
I hope this allows for a smoother way to following the different project steps.
On the other hand, this has significantly blown up the size of this project.
Also, you must change the code accordingly, if you want to work with your 
own calculated results.

The price data from https://creativecommons.tankerkoenig.de/
are a special case. If you want to run the code on the full data,
you must set up a Postgres server and load te two databases provided on the website
into the (not fully properly named, as the data cut is in May 2016) databases
benzin_2015, benzin_2016.
I found this relatively easy to do in Linux with some references back to Stackoverflow,
so have not included a tutorial on this topic.
Following the philosophy sketched above, you can also skip the Postgres and use the 
processed data set "D_pricesAgg_30_KEEP.rds" for further analysis.


