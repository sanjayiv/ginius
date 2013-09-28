
education_levels = {"completed college":8, "completed high school":5, "attended college":6, "completed graduate school":10, "attended vocational/technical": 5}

children_levels = {"no":1, "yes":2}

gender_levels = {"male":1, "female":2}

agerange_levels = {"18-20":19, "21-24":22.5, "25-34":29.5, "35-44":39.5, "45-54":49.5, "55-64":59.5, "65+":65}

homemarketvalue_levels = {"300k-350k": 325, "350k-500k":375, "500k-1mm":750, "50k-75k":62.5, "75k-100k":87.5, "100k-150k":125, "150k-200k":175, "1k-25k":13, "1mm+":1000, "200k-250k":225, "250k-300k":275, "25k-50k":37.5}

homeownerstatus_levels = {"own":2, "rent":1}

householdincome_levels = {"0-15k":12.5, "100k-125k":112.5, "125k-150k":137.5, "150k-175k":162.5, "15k-25k":20, "250k+":250, "25k-35k":30, "35k-50k":42.5, "50k-75k":62.5, "75k-100k":87.5}

lengthofresidence_levels = {"1 year": 1, "less than 1 year":0.5, "7 years":7, "4 years":4, "20+ years":20, "16-19 years":17.5, "8 years":8, "3 years":3, "2 years":2, "5 years":5, "11-15 years":13, "9 years":9, "10 years":10, "6 years":6, "38.1075":38}         

maritalstatus_levels = {"married":1, "single":2}


def normalizer(record, feature, feature_levels, default=0):
    return feature_levels.get(record[feature],default)

def normalize_age(record, feature):
    try:
        return float(record[feature])
    except:
        return 0
    return 

def socially_active(record, newrecord):
    feature = 'socially_active'
    #
    try:
        value = int(record['hasfacebook'] or '0') + int(record['haslinkedin'] or '0')
    except:
        import pdb; pdb.set_trace()
    return (feature, value)

def business_age_lie(record, newrecord):
    feature = 'business_age_lie'
    value = 0
    #
    age = newrecord["age"] or newrecord["agerange"]
    businessage = newrecord["businessage"]
    if age != 0 and businessage != 0:
        diff = age-businessage
        if diff < 10:
            value = 10
        elif diff < 15:
            value = 7
        elif diff < 18:
            value = 5
    return (feature, value)

