from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import pandas as pd
import sqlite3
conn = sqlite3.connect("/Users/andresgf91/Capstone/Data/SQL_TEXAS_HARVEY.sqlite")
import pickle

data = pd.read_sql_query("SELECT id_str,created_at,text FROM new_target_tweets;", conn)
conn.close()
print("data retrieved")

print("dataset has: " + str(len(data)) + " rows")

sentences = data['text']
analyzer = SentimentIntensityAnalyzer()
vs = list()
count=0
for sentence in sentences:
    count +=1
    vs.append(analyzer.polarity_scores(sentence)['compound'])
    if count % 100000 == 0:
        print(str(round(count/len(sentences)*100,2)) + "% completed")
print('Vader Scoring Complete')

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

become_a_pickle(vs,"/Users/andresgf91/Capstone/Data/vader_py_.pickle")
                
print("FINISHED!")