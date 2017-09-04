import re
import numpy as np
import requests
from bs4 import BeautifulSoup

def find_urls(url,crit_name):
    """
        args:
            url: url means to 
    """
    import requests
    from bs4 import BeautifulSoup
    page=requests.get(url).text
    pagesoup=BeautifulSoup(page,'lxml')
    url_list = []
    for link  in pagesoup.find_all(name='a',attrs={"href":re.compile(r'^'+crit_name)}):
        url_list.append(link.get('href'))
    return url_list

def find_design(url):
    import requests
    import numpy as np
    response = requests.get(url)
    text = list(response.text)
    tmp_list = []
    return_list = []
    try:
        for i in range(len(text)):
            if text[i] >= '0' and text[i] <= '9':
                if i<len(text)-1 and text[i+1] >= '0' and text[i+1] <= '9':
                    tmp_list.append(int(text[i]+text[i+1]))
                    text[i+1] = ' '
                else:
                    tmp_list.append(int(text[i]))
            elif text[i] == '\n':
                return_list.append(tmp_list)
                tmp_list = []
            if i == len(text)-1 and len(tmp_list) != 0 :
                return_list.append(tmp_list)
    except:
        print("The page displays 'HTTP 404.0 - Not Found' ")
        
    return return_list

def save_design(design,name,save_path = './design_data/'):
    """
    Save design in the given save_path.
    args:
        design: experimental design.
        name: file name.
        save_path: path to save the design
    """
    import numpy as np
    #if design = [], then we manually save 0 to csv.
    if(len(design) != 0):
        _design = np.array(design,dtype = np.int32)
    else:
        _design = np.array(0,dtype = np.int32)
    
    np.savetxt(save_path+name+".csv", _design, delimiter=",")

def find_design_size(url,crit_type="MD2_nq"):
    """
    Function reutrns run, factor and level of design in a given url.
    crit_type=MD2_nq means general design for mixture discrepancy.
    crit_type=MD2_nn means Latin hypercube design(LHD) for mixture discrepancy.
    so on and so forth.
    """
    if crit_type == "MD2_nq":
        Level = "Level%20(.*)/"
        Factor = "Level.*/(.*)_"
        Run = "_(.*).txt"
    elif crit_type == "MD2_nn":
        Level = "u_(.*)%5E"
        Factor = "Un_n%5Es_2/.*%5E(.*).txt"
        Run = Level
    elif crit_type == "CL2_nn":
        Level = "U_(.*)%5E"
        Factor = "Un_n%5Es/.*%5E(.*).txt"
        Run = Level
    elif crit_type == "CL2_nq":
        Level = "Level%20(.*)/"
        Factor = "Level.*/(.*)_"
        Run = "_(.*).txt"
        
    pLevel = re.compile(Level)
    pFactor = re.compile(Factor)
    pRun = re.compile(Run)
    level = pLevel.search(url).group(1)
    factor = pFactor.search(url).group(1)
    run = pRun.search(url).group(1)
    return(int(run),int(factor),int(level))


