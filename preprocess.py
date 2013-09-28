import csv
import sys
import optparse
from preprocessors import *

preprocess_config = { \
        "selectors": ["default_flag", "mentions", "twitterfollowers", "twitterfollowing", "validphone", \
        "verifiedemailaccount", "verifiedmerchant", "educations", "sum_haspresence", "sum_verified_aaeep", "sum_verifiedall", \
        "attributecredibility", "matchcount", "character", "matchquality", "consistency"],
        "mappers": [("education",education_levels), ("gender",gender_levels), ("agerange",agerange_levels), ("homemarketvalue",homemarketvalue_levels), \
                ("homeownerstatus",homeownerstatus_levels), ("householdincome",householdincome_levels), ("lengthofresidence",lengthofresidence_levels), \
                ("maritalstatus",maritalstatus_levels), ("children",children_levels)],
        "normalizers": [("age",normalize_age), ("businessage",normalize_age)],
        "derivedones": [business_age_lie]
        }

def preprocess(record, config):
    newrecord = {}
    features = []
    for feature in config["selectors"]:
        newrecord[feature] = record[feature] or "0"
        features.append(feature)
    for feature, feature_levels in config["mappers"]:
        newrecord[feature] = normalizer(record,feature,feature_levels)
        features.append(feature)
    for feature, normalizer_fn in config["normalizers"]:
        newrecord[feature] = normalizer_fn(record,feature)
        features.append(feature)
    for function in config["derivedones"]:
        feature, value = function(record, newrecord)
        newrecord[feature] = value
        features.append(feature)
    return (features, newrecord)

def preprocesscsv(csvfile,csvoutfile,config,max_records=0):
    processed_records = []
    fieldnames = None
    num_records = 0
    with open(csvfile, 'rb') as csvobj:
        reader = csv.DictReader(csvobj)
        for record in reader:
            features, processed_record = preprocess(record,config)
            if not fieldnames:
                fieldnames = features[:]
            processed_records.append(processed_record)
            num_records += 1
            if max_records and num_records >= max_records:
                break
    header = dict(zip(fieldnames,fieldnames))
    processed_records.insert(0,header)
    with open(csvoutfile, 'wb') as csvobj:
        writer = csv.DictWriter(csvobj,fieldnames)
        for record in processed_records:
            writer.writerow(record)
    return

def main(infile, outfile):
    preprocesscsv(infile,outfile,preprocess_config,0)

def parse_args():
    parser = optparse.OptionParser()
    parser.add_option("-i", "--input", default=None, help="Input")
    parser.add_option("-o", "--output", default=None, help="Output")
    (options, args) = parser.parse_args()
    if not (options.input and options.output):
        parser.print_help()
        sys.exit(1)
    return (options, args)

if __name__ == '__main__':
    try:
        options, args = parse_args()
        main(options.input, options.output)
    except SystemExit, ee:
        if 1 == ee.code:
            print "Error: Mandatory arguments missing!!"
        else:
            print str(ee)
    except Exception, ee:
        import traceback
        print traceback.print_exc()

