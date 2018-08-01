

# Asterisk
# 흔히 알고 있는 *를 의미함
# 단순 곱셉, 제곱연산 가변인자 활용 등 다양하게 사용됌


def asterisk_test(a, *args):
    print(a, args)
    print(type(args))

asterisk_test(1,2,3,4,5)  # tuple 타입으로 출력된다.

def asterisk_test2(a, **kargs): # 키워드 인자를 넘길땐 **
    print(a, kargs)
    print(type(kargs))

asterisk_test2(1, b=2, c=3, d=4, e=4, f=6)  # dict 타입으로 출력된다.


# tuple, dict 등 자료형에 들어가 있는 값을 unpacking
# 함수의 입력값, zip 등에 유용하게 사용가능

def asterisk_test3(a, args):
    print(a, *args)
    print(type(args))

asterisk_test3(1, (2,3,4,5,6)) # 언패킹이 되어서 프린트될땐 풀려진다.

a, b, c = ([1,2], [3, 4], [5, 6])
print(a,b,c)

data = ([1,2], [3, 4], [5, 6])
print(*data)


for data in zip(*([1,2], [3,4],[5,6])):
    print(sum(data))



    ## Data structure collections
    # 자료구조 기본 모듈
    # List, Tuple, Dict 에대한 python Built-in 확장 자료구조
    # deque : stack 과 Queue를 지원하는 모듈
    # list에 비해서 효율적인 저장 방식을 지원 속도도 빨름.


    # OrderedDict; dict와 달리 데이터를 입력한 순서대로 dict를 반환함.
    # dict는 순서대로 저장하지 않기때문에 순서대로 저장하는 모델인 Orderdict를 사용.

    #Defaultdict  기본 값을 지정. 초기값 없이 사용하게될때 사용된다.

    from collections import defaultdict

   # word_count = defaultdict(object)
    #word_count = defaultdict(lambda:1) # Default 값을 0으로 설정함
    #for word in text:
     #   word_count[word] += 1
      #  for i v in OrderDict(sorted(
        #    word_count.items(), key= lambda t: t[1], reverse] T

# Counter  Sequence type의 data elemenet들의 갯수를 dict 형태로 변황
#namedtupel:저장되는 data의 >>..?? 설명 너무빨름....


## Pythonic Code - Linear algebara codes (새로운 강의)
#Vector 를 파이썬으로 표시하는 다양한 바업ㅂ list, dict, list
# 기본적으로 list 으로 사용함.

# 벡터합
u = [2, 2]
v = [2, 3]
z = [3, 5]
result = [sum(t) for t in zip(u,v,z)]
print(result)

# Scalar = Vector product
u = [1, 2, 3]
v = [4, 4, 4]
print([5*sum(z) for z in zip(u,v)])

# 매트릭스는 list 타입으로 해준다.
matrix_a = [[3, 6],[4, 6]]
matrix_b = [[5, 8], [6,7]]
result = [[sum(row) for row in zip(*t)] for t in zip(matrix_a, matrix_b)]
print(result)   # zip 함수와 list 축약형을 이용하여 2차원 벡터합이 가능하다.

matrix_a =[[1,2,3,], [4,5,6]]    #matrix transpose 환성된다.
result = [[ element for element in t] for t in zip(*matrix_a)]
print(result)


# matrix product

matrix_a = [[1,1,2], [2,1,1]]
matrix_b = [[1,1], [2,1], [1,3]]
result = [[sum(a*b for a, b in zip(row_a, column_b)) \
           for column_b in zip(*matrix_b)] for row_a in matrix_a]

# Case Study - News Categorization (새강의) 40분 강의
# 비슷한 기사는 어떻게 모을까?
# 문자를 vetor로 바꿀줄 알아야한다. One-hot Encoding
# 하나의 단어를vector이 인덱스로 인식,
# 단어 존재시 1 없으면 0
# 코퍼스? 모든 단어를 가지고있는 집합
# 그 단어를 dict로 가지고있을때 사용하려면 1, 안하려면 0 으로 반환하는 등 한다.

#유사성은/ 피타고라스(유클리드 거리)  but 문서 비교할때는 코사인 시밀러러러티를 비교.
# 두점사이의 각도 cosine distance A*B = AB cos(theta)
# 데이터가 크면글수록 cosine distance가 더 잘나온다.


#Procoess
# 파일 불러오기 -> 파일을 읽어서 단어사전(corpus)만들기
# 단어별로 index 만들기 -> 만들어진 인덱스로 문서별로 Bag of word vector 생성
# 비교하고자하는 문서 비교하기 -> 얼마나 맞는지 측정하기.


