# ContinuousConsistentVoting

Thank you for your interest in replicating our analyses! We hope you encounter no difficulties in doing so, and have striven to make
this process as straightforward as possible. Do bear in mind that replicating our analyses may take you some time due to the necessity
of downloading a number of different datasets from different websites, and the fact that these scripts can take a long time to run due 
to the large size of the data objects they use. 

*To replicate the analyses and reproduce the graphs in full:*

    1. You will need to download the data sources using the information provided in the paper (we lack permission to share them ourselves)
        For the sake of ease we repeat the locations of these datasets at the bottom of this file.
        Ensure to align the names of the source files you save with those in the scripts 
        Also bear in mind that depending on your software and settings, your table and spreadsheet headings may differ to ours
        Remove superfluous headers if you have any difficulties running the code dealing with third-party data sources.
     
    2. You will need to download PPMatrix_trim.csv, available on the github page

    3. Run the scripts in numerical order

    4. Run our stata do files on the 'cut_for_stata' files produced in the sixth script, also available on the github page.
    
       Note that the alternative construction referred to in the paper as 'All-Estimate' is referred to as '_Unprejudiced' in the code

*To just replicate the final multi-level regression analysis:*

    You will need to download the following data files and run the stata do files available on the github page:
        https://drive.google.com/file/d/1tCFID4dgHX_LMqDmVuHwwcRi4KNpe-bZ/view?usp=sharing
        https://drive.google.com/file/d/1tE7IVjmUQMLZUO3w-f9nqqYW9YZxwJvm/view?usp=sharing
        https://drive.google.com/file/d/1tFo84IdvCi3i9klmGlEiBlY7WwCMbRsD/view?usp=sharing
        
*To just replicate the continuous consistency calculation:*

    You will need to download the following data files and run the sixth script:
        https://drive.google.com/file/d/1t1B4K72OZV6QT7Okud6_z9j-xRDESESQ/view?usp=sharing
        https://drive.google.com/file/d/1t1EIpi1QHZ8BTntkkqzIcyM_ytJ3FZ9U/view?usp=sharing
        
We share these files here as they are too large to host on github.

Contact David Young (dy286@cam.ac.uk) if you have any difficulties running these scripts, or other related queries or suggestions.

------------------------------------------------------------------------------------------------------------------------------

*Required datasets for a full replication*:

       Incentives to Cultivate a Personal Vote: Johnson, J. W. & Wallack, J. S. (2012). 
       Electoral Systems and the Personal Vote. Harvard Dataverse, V1. Retrieved from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AMRXJA 

       Comparative Study of Electoral Systems:  
       Module 1: 	
       Comparative Study of Electoral Systems Secretariat. COMPARATIVE 
       STUDY OF ELECTORAL SYSTEMS, 1996-2001 [Computer file]. 4th 
       ICPSR version. Ann Arbor, MI: University of Michigan, Center for Political Studies [producer], 2002. Ann Arbor, MI: Inter-university Consortium for Political and Social             Research [distributor], 2004. 
       Module 2: 
       Sapiro, V, and Shively, W. P. Comparative Study of Electoral Systems, 2001-2006. Ann Arbor, MI: Inter-university Consortium for Political and Social Research                       [distributor], 2008-07-01. https://doi.org/10.3886/ICPSR03808.v2 
       Module 3: 
       The Comparative Study of Electoral Systems (www.cses.org). CSES MODULE 3 FULL RELEASE [dataset]. December 15, 2015 version. 
       doi:10.7804/cses.module3.2015-12-15 
       Module 4: 
       The Comparative Study of Electoral Systems (www.cses.org). CSES MODULE 4 FULL RELEASE [dataset]. May 29, 2018 version. doi:10.7804/cses.module4.2018-05-29 
       Module 5: 	
       The Comparative Study of Electoral Systems (www.cses.org). CSES MODULE 5 FIRST ADVANCE RELEASE [dataset]. May 21, 2019 version. doi:10.7804/cses.module5.2019-05-21 
       
       Database of Political Institutions: Cruz, C., Keefer, P., & Scartascini, C. (2018). 
       Database of Political Institutions 2017 (DPI2017). Inter-American Development Bank. Numbers for Development. https://mydata.iadb.org/Reform-Modernization-ofthe-State/               Database-of-Political-Institutions-2017/938i-s2bw  
       
       Gallagher (2020) Least Squares Index pdf:  https://www.tcd.ie/Political_Science/people/michael_gallagher/ElSystems/Docts/Elec tionIndices.pdf 
       
       Media Density data:  
       Internet: 
        	Fixed Broadband Subscriptions (per 100 people). [dataset]. 
       World Bank. International Telecommunication Union, World Telecommunication/ICT Development Report and database. 
       Retrieved from: https://data.worldbank.org/indicator/IT.NET.BBND.P2 
       
       Mobiles phones: 	
       Mobile cellular subscriptions (per 100 people). [dataset]. World 
        Bank. International Telecommunication Union, World 
        Telecommunication/ICT Development Report and database. 
            Retrieved from: 
        https://data.worldbank.org/indicator/IT.CEL.SETS.P2 
        
        Newspapers:  	
        Daily newspapers: Total Average Circulation per 1000 
        Inhabitants. [dataset]. UNESCO Institute for Statistics. Retrieved from: 
        http://data.un.org/Data.aspx?d=UNESCO&f=series%3AC_N_C DN 
         
        Polity IV: 
        Marshall, M. G. & Gurr, T. R. (2013). Polity IV Project: Political Regime Characteristics and Transitions, 1800-2013. Political Instability Task Force                          (Central Intelligence Agency). Retrieved from: https://www.systemicpeace.org/polity/polity4.htm 
        
        World Bank GDP data: 
        GDP growth (annual %). World Bank National Accounts Data, and OECD National Accounts Data Files. Retrieved from:                                                                    https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG 


