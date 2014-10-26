
import os

os.chdir('/home/roberto/Documents/bertplot/data/bmi extract')

def createHTML(year):
    i = 0
    output = '<div class="fattest" style="font: normal normal normal smaller/1.3em Helvetica,sans-serif;">\n'
    output += '<table style="border: 2px solid #cccccc; text-align: center; border-collapse: separate; border-spacing: 4px; width: 440px; margin: 1em 0;" border="0"><caption style="font-size: larger; font-weight: bold; margin: 1.2em 0 0.3em 0;">The CalorieLab United States of Obesity Fattest States Ranking %s</caption>\n'% (year+1)
    output += '<tbody>\n'
    output += '<tr style="background-color: #bbbbbb;">\n'
    output += '<th>%s\n'%year
    output += 'Rank</th>\n'
    output += '<th>%s\n' % (year+1)
    output += 'Rank</th>\n'
    output += '<th>State</th>\n'
    output += '<th>% Obese\n'
    output += '%s</th>\n'%year
    output += '<th>% Obese or\n'
    output += 'Overweight\n'
    output += '%s</th>\n'%year
    output += '<th>3-year\n'
    output += 'Obesity\n'
    output += 'Average</th>\n'
    output += '<th>% Obesity\n'
    output += 'Change</th>\n'
    output += '<th>Ranking\n'
    output += 'Change</th>\n'
    output += '</tr>\n'    
    with open('output-%s.csv'%year, 'r') as infile:
        for line in infile:
            if 'rank' in line: continue
            else:
                #print line
                i+=1
                parsed = line.split(',')
                print float(parsed[5])
                if i == 1:
                    output +='<tr style="background-color: #ffaaaa;">\n'
                elif float(parsed[5]) >= 31:
                    output +='<tr style="background-color: #ffdddd;">\n'
                elif 29 <= float(parsed[5]) <= 31:
                    output +='<tr style="background-color: #ffeeee;">\n'    
                elif 27 <= float(parsed[5]) < 29:
                    output +='<tr style="background-color: #ffeedd;">\n'              
                elif 25 <= float(parsed[5]) < 27:                    
                    output +='<tr style="background-color: #ffffdd;">\n'  
                elif float(parsed[5]) <  25:                    
                    output +='<tr style="background-color: #ddffdd;">\n'                     
                elif i == 51:
                    output +='<tr style="background-color: #aaffaa;">\n'                                     
                output +='<td>%s</td>\n' % parsed[0]
                output +='<td style="font-weight: bold;">%s</td>\n' % parsed[1]
                output +='<td style="text-align: left; font-weight: bold;">%s</td>\n' % parsed[2]
                output +='<td>%s</td>\n' % parsed[3]
                output +='<td>%s</td>\n' % parsed[4]
                output +='<td>%s</td>\n' % parsed[5]
                output +='<td>%s</td>\n' % parsed[6]
                output +='<td>%s</td>\n' % parsed[7]
                output +='</tr>\n'
    with open('final-table-%s.html'%(year+1),'w') as out:
        out.write(output)
        
createHTML(2013)
createHTML(2012)
createHTML(2011)
