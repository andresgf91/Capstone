import pandas as pd
import numpy as np
import pickle
from collections import Counter

def main():
    pass


def become_a_pickle(data,file_name):
    '''Stores data in a pickle file'''
    with open(file_name, 'wb') as file:
        pickle.dump(data,file, protocol=pickle.HIGHEST_PROTOCOL)
    print('Python',type(data),'object has been pickled')
    
def serve_pickle(file_name):
    '''Retrieves data stored in a pickle file
    returns python object originally saved in pickle file'''
    with open(file_name, 'rb') as file:
        data = pickle.load(file)
    return(data)


def get_data_list(file,delimiter='\n',dtype=str):
        '''Returns list containing data in file.
        Default is set to parse data of type "string",delimited by new lines.
        For alternative see numpy.loadtxt documentation'''
        
        ls=(np.loadtxt(file,delimiter=delimiter,dtype=dtype)
            .tolist())
        
        #check if there are duplicates in list
        if (len(ls))-(len(set(ls)))!=0:
            print(file,'file contains:',
                  (len(ls))-(len(set(ls))),'duplicates')
        
        return(ls)


def rows_per_group(df,group_col,count_name='count'):
    '''returns a frequency table (as pd DataFrame)
    indicating number of rows in df for each group in group_col'''

    df=(pd.DataFrame(df[group_col]
        .value_counts())
        .reset_index()
        .rename(columns={group_col:count_name,'index':'user'}))

    return(df)

    
def colsum_per_group(df,groupby,sum_col):
    '''returns a pandas df with two columns:
    Column in index 0 - group columns
    Column in index 1 - sum of values in column of interest 

    Takes three arguments: 
    1) df - pandas dataframe
    2) grouby - name of group column
    3) sum_col - name of column with values to sum 

    assumptions: 
     * `groupby` and `sum_col` arguments are strings
     * `sum_col` contains integers or float
    '''
           
    df=(pd.DataFrame
        (df.groupby([groupby],sort=False)
         [sum_col].sum())
        .reset_index())
    
    return(df)

        
      
def at_least_once(df,colA,colB,x=0):
    '''returns a list containing values in colA
    where values in colB are greater than x

    Takes 4 arguments: 
    1) df - pandas dataframe
    2) ColA - name of group column
    3) ColB - name of column with values to sum
    4) x - numeric value (default=0)
    '''
    ls=((df[df[colA]>x])[colB].tolist())
    
    return(ls)

           
def subset_A_in_ls(df,colA,ls):
    '''Returns subset of df
    keeping only rows where values in colA appear in ls
    Assumes df is a pandas DataFrame'''
    df=df.loc[df[colA].isin(ls)]
    return(df)
        
def col_to_ls(df,col):
    '''Returns values in df column col as a list.
    Assumes df is a pandas DataFrame'''
    ls=df[col].tolist()
    return(ls)

     
def pd_network_nodes(df,node_cols):
    '''Returns set of unique nodes in network DataFrame
    Function takes two arguments: 
    1) df - a pandas DataFrame with network nodes data
    2) node_cols - list of columns names containing network nodes
    '''
    ls=pd.melt(df,value_vars=node_cols)['value'].tolist()

    return(set(ls))
        

def pd_network_summary(df):
    '''Prints number of Edges, Nodes in network
    Also prints number of unique nodes in vertex1 and vertex2
    Assumes df is a Pandas Dataframe with columns ordered as follows:
    ['vertex_source','edge','vertex_target']'''
    
    names=list(df)
    nodes=pd_network_nodes(df,[names[0],names[2]])

    print('The number of edges in the network (reverts) is:',len(df))
    print('The number of nodes in the network is:',len(nodes),
          '\n ','\n ',
          df.describe().iloc[0:2,[0,2]])


def values_count(dic):
    '''Assumes dic is is dictionary object
    
    Returns Counter object where:
    Key - Values in dic 
    Values - Number of times each value appears in dic.
    '''
    ls=dic.values()
    group_count=Counter(ls)

    return(group_count)        


if __name__=='__main__':
    main()