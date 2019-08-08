from os import walk
import gzip
import shutil
import json
import pickle

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

def get_file_names(path):
    f = []
    for (dirpath, dirnames, filenames) in walk(path):
        f.extend(filenames)
        try:
            f.remove('.DS_Store')
        except ValueError:
            pass
        return (f)

def split_jeison(file,save_to_path,n_files=150,value='1_'):
    with open(file) as infp:
        files = ([open(save_to_path+'/'+value+'%d.json' % i, 'w') 
                    for i in range(n_files)])
        for i, line in enumerate(infp):
            files[i % n_files].write(line)
        for f in files:
            f.close()
    print('Finished splitting file' + file + '...')
    print('The file was split into: ' + str(n_files) + ' smaller files')
    print('Your split files are saved in: ' + save_to_path)
    
    
def split_jeison_gzip(file,save_to_path,n_files=150,value='1_'):
    with gzip.open(file) as infp:
        files = ([open(save_to_path+'/'+value+'%d.json' % i, 'wb') 
                    for i in range(n_files)])
        for i, line in enumerate(infp):
            files[i % n_files].write(line)
        for f in files:
            f.close()
    print('Finished splitting file' + file + '...')
    print('The file was split into: ' + str(n_files) + ' smaller files')
    print('Your split files are saved in: ' + save_to_path)
    
    
    
def split_jeisitos_from_folder(folder,save_to_path,n_files=150):
    jeisons_files=get_file_names(folder)
    for i in jeisons_files:
        file=(folder+'/'+i)
        print('spliting files in '+file+'...')
        val=(i[:-5]+'_')
        split_jeison(file,save_to_path,n_files=n_files,value=val)
    print('Finished spliting all .json files in folder: ' + folder)


    
def get_texas_tweets_gzip(file,save_to_path,file_num,pickled_path,location_names):
    # initialize counter objects to zero
    place_count = 0
    geo_count = 0
    coordinates_count = 0
    user_location_count = 0
    
    with gzip.open(file) as infp:
        # create file name
        file_name = open((save_to_path + '/TEXAS' + str(file_num) +'.json'), 'wb')
        
        for i,line in enumerate(infp):
            try:
                d = json.loads(line) #convert line into dictionary object
            except ValueError:
                continue
            #look in Place field    
            try:
                if d['place']!= None: #check if place is not-null
                    if d['place']['country_code'] == 'US' and \
                    d['place']['full_name'] in location_names:             
                        file_name.write(line)
                        place_count += 1
                        continue
            except (KeyError,TypeError) as e:
                pass     
            # look in geo field
            try:
                if d['geo']!=None:
                    if 27 < d['geo']['coordinates'][0] < 31 \
                    and -97.85 < d['geo']['coordinates'][1] < -93.50:            
                        file_name.write(line)
                        geo_count += 1
                        continue
            except KeyError:
                pass
            #look in coordinates field
            try:
                if d['coordinates']!=None:
                    if -97.85 < d['coordinates']['coordinates'][0] < - 93.50 \
                    and 27 < d['coordinates']['coordinates'][1] < 31:
                        file_name.write(line)
                        coordinates_count += 1
                        continue
            except (KeyError,TypeError) as e:
                pass
            #look in user_profile location field 
            try:
                if d['user']['location']!= None: #check if user_profile location is not-null
                    try:
                        if d['user']['location'].lower().find('houston') != -1 \
                        or d['user']['location'].lower().find('.tx') != -1 \
                        or d['user']['location'].lower().find('texas') != -1 \
                        or d['user']['location'].lower().find(',tx') != -1 \
                        or d['user']['location'].lower().find('htx') != -1:
                            file_name.write(line)
                            user_location_count += 1
                            continue
                    except AttributeError:
                        pass
            except (KeyError,TypeError) as e:
                pass

        file_name.close()
        
        # compute total count of SX-MF tweets
        texas = (place_count + geo_count + coordinates_count + user_location_count)
        
        #compute percentage of SX-MF tweets out of total tweets
        percentage = round(texas/i*100,ndigits = 4)
        
        #create dict object to store metrics as pickl
        metrics = {'percent':percentage,'place_count':place_count,
                   'geo_count':geo_count,'coordinates_count':coordinates_count,
                   'user_location_count':user_location_count,
                   'total_tweets':i,'total_texas':texas}
        
        #create pickle file
        pickle_file = (pickled_path + 'metrics_' + str(file_num) + '.pickle')
        become_a_pickle(metrics,pickle_file)
        
        #print messages at the end
        print('Finished looking for affected texas tweets in: ' + str(file))
        print(str(percentage) + ' percent of tweets are potentially from affected texas areas')
        print('.........')    
    
    
        
def Get_texas_from_folder_gzip(folder,save_to_path,pickled_path,location_names):
    jeisons_files = get_file_names(folder)
    for i in jeisons_files:
        file = (folder+'/'+i)
        print('Looking for affected texas tweets in: '+ file + '...')
        temp_file_name=(i[:-8])
        get_texas_tweets_gzip(file,save_to_path,file_num = temp_file_name,pickled_path = pickled_path,location_names = location_names)
    print('Finished finding all affected texas counties tweets from files in: ' + folder)

    
print('All functions loaded...')


if __name__=='__main__':
    main()