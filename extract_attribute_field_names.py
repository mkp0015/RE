import csv
import arcpy

csvLocation = r'C:\Users\m3rexmkp\Desktop\attribute_names.csv'
arcpy.env.workspace = r'C:\Users\m3rexmkp\Desktop\Database\coastal_tx_parcels.gdb'

#write header
with open(csvLocation,'wb') as openCSV:
    a = csv.writer(openCSV)
    message = [['FC','NAME', 'TYPE', 'LENGTH']]
    a.writerows(message)

    #Get list of feature classes in geodatabase
    FCs = arcpy.ListFeatureClasses()    

    #Loop through feature classes in list
    for FC in FCs:
        #List fields in feature class
        fields = arcpy.ListFields(FC)

        #Loop through fields and write to csv
        for field in fields:

            message = [[FC, field.name, field.type, field.length]]
            a.writerows(message)
