//
// copyright (c) Deel/linan
// Home:      https://github.com/DD-L/lexer
// License:   MIT
// 
#pragma once

#include <cctype>
#include <cstring>
#include <utility>
#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <vector>
#include <string>
#include <deque>
#include <assert.h>

#ifndef DDL_LEXER
#define DDL_LEXER DDL_LEXER
#endif // DDL_LEXER

namespace DDL_LEXER
{
    class Lexer;
   
    struct StrRef
    {
        const char* m_begin = nullptr;
        const char* m_end = nullptr;

    public:
        constexpr StrRef() = default;

        constexpr StrRef(const char* _str, std::size_t _len)
            : m_begin(_str), m_end(_str + _len)
        {}

        template <class Iter>
        constexpr StrRef(Iter&& begin, Iter&& end)
            : m_begin(&*begin), m_end(&*end)
        {
            assert(begin <= end);
        }

        template <std::size_t N>
        constexpr StrRef(const char (&arr)[N] )
            : m_begin(arr), m_end(arr + N - 1u)
        {
            static_assert(0 != N, "Error");
        }

        constexpr StrRef(const StrRef& strRef) noexcept
            : m_begin(strRef.m_begin), m_end(strRef.m_end)
        {}

        /*explicit*/ StrRef(const std::string& stdstr) noexcept
            : m_begin(stdstr.data()), m_end(stdstr.data() + stdstr.size())
        {}

        template <class T, 
            class = typename std::enable_if<
             std::is_same<T, int8_t>::value || std::is_same<T, uint8_t>::value>::type>
        /*explicit*/ StrRef(const std::vector<T>& stdvec) noexcept
            : m_begin((char*)stdvec.data()), m_end((char*)stdvec.data() + stdvec.size())
        {}

        // For Range
        const char* begin() const { return m_begin; }
        const char* end()   const { return m_end; }

        bool IsNull() const
        {
            assert((nullptr == m_begin) ? (0 == m_end) : true);
            return nullptr == m_begin;
        }

        std::size_t Length() const
        {
            assert(m_begin <= m_end);
            return m_end - m_begin;
        }

        std::string ToStdString() const
        {
            return std::string(m_begin, m_end);
        }

        const char* operator+(std::size_t offset) const noexcept
        {
            return offset <= Length() ? (m_begin + offset) : nullptr;
        }

        char operator[](std::size_t offset) const noexcept
        {
            assert(offset < Length());
            return *((*this) + offset); // may crash ...
        }
    }; //  StrRef

    // forward decl 
    class VariableAllocator;

    class VariableType
    {
    public:
        bool IsNormal()     const  noexcept { return !IsMutable(); }
        bool IsMutable()    const  noexcept { return 0 != TopBit(); }
        bool IsLoop()       const  noexcept { return  Classify::_Loop == WipeTop(); }
        bool IsBranch()     const  noexcept { return Classify::_Branch == WipeTop(); }
        bool IsSequence()   const  noexcept { return Classify::_Sequence == WipeTop(); }
        bool IsTerminator() const  noexcept { return Classify::_Terminator == WipeTop(); }
        bool IsDollarFunc() const  noexcept { return Classify::_DollarFunc == WipeTop(); }

        void SetNormal()     noexcept { m_flag = WipeTop(); }
        void SetMutable()    noexcept { m_flag |= (~(Classify::classMask)); }
        void SetLoop()       noexcept { m_flag = KeepTop() | Classify::_Loop; }
        void SetBranch()     noexcept { m_flag = KeepTop() | Classify::_Branch; }
        void SetSequence()   noexcept { m_flag = KeepTop() | Classify::_Sequence; }
        void SetTerminator() noexcept { m_flag = KeepTop() | Classify::_Terminator; }
        void SetDollarFunc() noexcept { m_flag = KeepTop() | Classify::_DollarFunc; }

        void SetTypeKeepingTop(const VariableType& subVarType) noexcept
        {
            m_flag = KeepTop() | subVarType.WipeTop();
        }

        void Reset() noexcept
        {
            m_flag = 0;
        }

    private:
        uint8_t WipeTop() const noexcept { return (m_flag & Classify::classMask); }
        uint8_t KeepTop() const noexcept { return  m_flag & (~(Classify::classMask)); }
        uint8_t TopBit()  const noexcept { return KeepTop() >> 7; }

    private:
        enum Classify : uint8_t
        {
            _Branch     = 1,  _Sequence   = 2,
            _Loop       = 3,  _Terminator = 4,
            _DollarFunc = 5,  classMask   = 0x7f,
        }; // enum Flag

        uint8_t m_flag = 0;
    }; // class VariableType

    struct Ingredients
    {
        std::size_t start = 0;
        std::size_t offset = 0;
        std::size_t constraint = 0;
        uint64_t    data = 0;

        void Clean() noexcept
        {
            assert(start <= offset);

            start = offset;
            constraint = 0;
            data       = 0;
        }
        
        void GoBack() noexcept
        {
            assert(start <= offset);
            GoBack(start);
        }

        void GoBack(std::size_t _start) noexcept
        {
            assert(_start <= offset);

            Clean();
            start  = _start;
            offset = _start;
        }
    }; // struct Ingredients

    class Food final
    {
    public:
        ~Food() {}
        Food() = default;
        Food(StrRef str, const DDL_LEXER::VariableType& type, 
            std::size_t constraint, uint64_t ctxInt)
            : m_str(str), m_constraint(constraint), m_ctxInt(ctxInt), m_type(type)
        {
        }

    public:
        void* Data() const { return m_data; }
        const StrRef& Str() const { return m_str; }
        const char* Begin() const { return Str().begin(); }
        const char* End() const { return Str().end(); }
        std::size_t Length() const { return Str().Length(); }
        
        std::size_t BranchID() const
        {
            assert(m_type.IsBranch());
            return m_branchID;
        }

        std::size_t LoopCount() const
        {
            assert(m_type.IsLoop());
            return m_loopCnt;
        }

        const DDL_LEXER::VariableType& VariableType() const
        {
            return m_type;
        }

        static uint64_t AsInt(void* data) noexcept 
        { 
            return reinterpret_cast<uint64_t>(data); 
        }

    protected:
        StrRef  m_str;

        union
        {
            std::size_t m_constraint = 0;
            std::size_t m_branchID;
            std::size_t m_seqSize;
            std::size_t m_loopCnt;
        };

        union
        {
            uint64_t m_ctxInt = 0;
            void*    m_data;
        };

        const DDL_LEXER::VariableType  m_type;
    }; // class Food

    class Action
    {
    public:
        virtual ~Action() {}
        virtual bool Handler(const Food& food, void* ctx, std::string&) = 0;
        virtual bool RealTimeHandler(const Food& food, void* ctx, std::string& err) noexcept { return true; }
    }; // class Action

    struct ActionQueueEle
    {
        Action* action = nullptr;
        Food    food;

        ActionQueueEle() = default;
        ActionQueueEle(Action* _ac, Food&& _food) : action(_ac), food(std::move(_food)) {}
    }; // struct ActionQueueEle

    typedef std::vector<ActionQueueEle> ActionQueue;
    typedef std::unordered_map<std::string, Action*> ActionTable;

    class Variable;
    typedef std::unordered_map<std::string, Variable*>  VariableTable;

    // Syntax Variable 
    class Variable
    {
    public:
        Variable() = default;
        //explicit Variable(VariableType type)
        //    : m_type(type)
        //{}
        //
        virtual ~Variable() {
            m_mut = nullptr;
            m_type = VariableType();
        }

        virtual bool Scan(const StrRef& script, Ingredients& ingredients,
            ActionQueue& actions, void*, std::string& err) noexcept = 0;  // 空串使用 '' 表示

        bool IsMutable() const
        {
            return m_type.IsMutable();
        }

    public:
        void SetAction(Action* action)
        {
            m_action = action;
        }

        Action* GetAction() const
        {
            return m_action;
        }

        const VariableType& Type() const
        {
            return m_type;
        }

    public:
         void _SetName(StrRef name) noexcept
         {
             m_varName = name;
         }

         const StrRef& _Name() const
         {
             return m_varName;
         }

         void _SwapMut(Variable* newMut, Variable*& oldMut = ThreadLocalDummyVariable()) noexcept
         {
             oldMut = m_mut;
             m_mut = newMut;
             if (nullptr != m_mut)
             { // update type
                 m_type.SetTypeKeepingTop(m_mut->Type());
             }
         }

    protected:
         bool PushAction(ActionQueue& actions, const StrRef& script,
             const Ingredients& ingredients,
             void* ctx, std::string& err) const noexcept
         {
             if (nullptr != m_action)
             {
                 Food food({ script + ingredients.start, script + ingredients.offset }, 
                     m_type, ingredients.constraint, ingredients.data);
                 if (m_action->RealTimeHandler(food, ctx, err))
                 { // “actions，data”和 “ctx” 适用范围不同， 不能将其 代入到 ctx 中
                     actions.emplace_back(m_action, std::move(food));
                     return true;
                 }
                 return false;
             }

             return true;
         }

         static Variable*& ThreadLocalDummyVariable() noexcept
         {
             thread_local Variable* dummy = nullptr;
             return dummy;
         }

        template <class Allocator>
        void ReshapeToMut(Allocator& allocator) noexcept;

        // 只需 内置 SyntaxVar 重写 _Move 即可
        virtual Variable* _Move(VariableAllocator& allocator) noexcept
        {
            return nullptr; (void)allocator;
        }

    protected:
        friend class Lexer;

        Variable*            m_mut = nullptr;  // 用作动态绑定，仅仅被用在 mut_var 的情况；
        // 但有必须写在基类中，原因是确保其他子类对象可以被安全的重塑为 MutableVariable 对象
        VariableType         m_type;
        StrRef               m_varName;
        DDL_LEXER::Action*   m_action = nullptr;
    }; // class Variable

    namespace internal
    {
        class MutableVariable;
    } // internal

    // 变量内存分配器，同时承担内存持有的职责
    class VariableAllocator
    {
    public:
        ~VariableAllocator() noexcept { Destroy(); }
        void Reset() noexcept { Destroy(); }

        template <class Syntax, class... Args>
        Syntax* Alloc(Args&&... args) noexcept
        {
            return ForceAlloc<Syntax>(std::forward<Args>(args)...);
        }

        template <class Syntax,
            class = typename std::enable_if<!std::is_same<Syntax, internal::MutableVariable>::value>::type>
        Variable* Alloc(Variable* v) noexcept
        {
            return v;
        }

        template <class Syntax, class... Args>
        Syntax* ForceAlloc(Args&&... args)
        {
            Syntax* var = new (std::nothrow) Syntax(std::forward<Args>(args)...);
            if (nullptr != var)
            {
                m_vars.push_back(var);
            }
            return var;
        }

    private:
        void Destroy() noexcept
        {
            for (Variable* v : m_vars)
            {
                delete v;
            }

            m_vars.clear();
        }
    private:
        std::deque<Variable*>  m_vars;
    }; // class VariableAllocator

    // 字符串字面值内存分配器
    class MemoryAllocator final
    {
    public:
        char* Alloc(std::size_t n)
        {
            char* ret = new char[n];
            m_holder.emplace_back(ret, n);
            return ret;
        }

        ~MemoryAllocator() noexcept
        {
            Reset();
        }

        void Reset() noexcept
        {
            for (StrRef& ele : m_holder)
            {
                delete[] ele.begin();
            }
            m_holder.clear();
        }

    private:
        std::vector<StrRef> m_holder;
    }; // MemoryAllocator

    namespace
    {
        struct RangeArg { constexpr RangeArg() = default; };
        constexpr RangeArg _range_arg;
    }

    namespace internal
    {
        //
        // 一种动态绑定的 Variable.
        // 其意义是尽可能的复用文法（运行期），或者前式声明（编译期）
        class MutableVariable : public Variable
        {
        public:
            explicit MutableVariable(Variable* mut = nullptr)
            {
                m_type.SetMutable();
                Set(mut);
            }

            bool Scan(const StrRef& script, Ingredients& ingredients,
                ActionQueue& actions, void* ctx, std::string& err) noexcept override
            {
                // 确保其他 Variable 衍生对象可以安全的重塑为 mut_var
                static_assert(sizeof(MutableVariable) == sizeof(Variable), "Error"); 

                assert(Valid());
                ingredients.Clean();
                if (m_mut->Scan(script, ingredients, actions, ctx, err))
                {
                    return PushAction(actions, script, ingredients, ctx, err);
                }

                return false;
            }

            bool Valid() const
            {
                return (nullptr != m_mut);
            }

            bool Set(Variable* newVar) noexcept
            {
                m_mut = newVar;
                return Valid();
            }
        }; // class MutableVariable

        // 终止符, 支持空串
        class SyntaxToken : public Variable
        {
            typedef SyntaxToken _Myt;

        public:
            SyntaxToken() { m_type.SetTerminator(); }
            explicit SyntaxToken(StrRef token) : m_token(token) { m_type.SetTerminator(); }

            [[nodiscard]] bool SetLeftWhites(Variable* whites) noexcept
            { // 要求：当前 Token 被当前设定的白字符从左到右 Scan，不能出现被消耗字符的情况
                Ingredients ingredients;
                if (nullptr != whites)
                { 
                    m_leftWhites = nullptr;

                    std::string e;
                    ActionQueue aq;
                    whites->Scan(m_token, ingredients, aq, nullptr, e); (void)aq, (void)e;
                }

                m_leftWhites = whites;
                return (0 == ingredients.offset); // 没有左交集
            }

            bool SetRightAssert(Variable* _Assert) noexcept
            {
                m_rightAssert = _Assert;
                return true;
            }

            virtual bool Scan(const StrRef& script, Ingredients& ingredients, ActionQueue& actions, void* ctx, std::string& err) noexcept
            {
                const std::size_t start = ingredients.offset; // origin
                ingredients.Clean();
                if (nullptr != m_leftWhites)
                { // white 不允许产生副作用，所以 context 必须为 空
                    std::string e;
                    ActionQueue aq;
                    m_leftWhites->Scan(script, ingredients, aq, nullptr, e); (void)aq, (void)e;
                    ingredients.Clean();
                }

                if (ScanImpl(script, ingredients.offset, ingredients.data, err))
                {
                    if (nullptr == m_rightAssert)
                    {
                        return PushAction(actions, script, ingredients, ctx, err);
                    }
                    // 对 Token 右边界的零宽断言，右边界不允许产生副作用，所以 context 必须为 空
                    Ingredients dummyIngredients; // 无副作用的食材
                    dummyIngredients.offset = ingredients.offset;
                    std::string e; 
                    ActionQueue aq;
                    if (m_rightAssert->Scan(script, dummyIngredients, aq, nullptr, e))
                    { 
                        (void)(e); (void)aq;
                        return PushAction(actions, script, ingredients, ctx, err);
                    }
                }
                ingredients.GoBack(start); // 恢复前面白字符的消耗 ？？ 有必要 ？？？？有！
                return false;
            }
        
            virtual bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string& err) noexcept
            {
                if ((offset + m_token.Length()) <= script.Length())
                {
                    if (0 == std::memcmp(script + offset, m_token.begin(), m_token.Length()))
                    {
                        offset += m_token.Length();
                        return true;
                    }
                }
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxToken* var = allocator.ForceAlloc<SyntaxToken>(m_token);
                this->~SyntaxToken();
                return var;
            }

        private:
            StrRef    m_token;
            Variable* m_leftWhites  = nullptr;
            Variable* m_rightAssert = nullptr;
        }; // class SyntaxToken

        // Basic Structures (syntax/sentence structure)
        class SyntaxSequence : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxSequence(Args&&... args)
                : m_sequence({ std::forward<Args>(args) ... })
            {
                m_type.SetSequence();
            }

            explicit SyntaxSequence(std::vector<Variable*>&& branch)
                : m_sequence(std::move(branch))
            {
                m_type.SetSequence();
            }

            template <class Iter>
            SyntaxSequence(RangeArg, Iter&& begin, Iter&& end)
                : m_sequence(begin, end)
            {
                m_type.SetSequence();
            }

            SyntaxSequence& AppendVariable(Variable* var)
            {
                m_sequence.push_back(var);
                return *this;
            }

            bool Scan(const StrRef& script, Ingredients& ingredients, ActionQueue& actions, void* ctx, std::string& err) noexcept override
            {
                ingredients.Clean();
                std::size_t cnt = 0;
                for (Variable* v : m_sequence)
                {
                    if (! v->Scan(script, ingredients, actions, ctx, err))
                    {
                        ingredients.GoBack();
                        return false;
                    }
                    ++cnt;
                }

                if (m_sequence.size() == cnt)
                {
                    ingredients.constraint = cnt;
                    return PushAction(actions, script, ingredients, ctx, err);
                }

                ingredients.GoBack();
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxSequence* var = allocator.ForceAlloc<SyntaxSequence>(std::move(m_sequence));
                this->~SyntaxSequence();
                return var;
            }

        protected:
            std::vector<Variable*>  m_sequence;
        }; // class SyntaxSequence

        // 有序分支结构
        class SyntaxBranch : public Variable
        {
        public:
            template <class... Args>
            explicit SyntaxBranch(Args&&... args)
                : m_branches({ std::forward<Args>(args) ... })
            {
                m_type.SetBranch();
            }

            template <class Iter>
            SyntaxBranch(RangeArg, Iter&& begin, Iter&& end)
                : m_branches(begin, end)
            {
                m_type.SetBranch();
            }

            explicit SyntaxBranch(std::vector<Variable*>&& branch)
                : m_branches(std::move(branch))
            {
                m_type.SetBranch();
            }

            void AppendVariable(Variable* var)
            {
                m_branches.push_back(var);
            }

            bool Scan(const StrRef& script, Ingredients& ingredients, ActionQueue& actions, void* ctx, std::string& err) noexcept override
            {
                const std::size_t start = ingredients.offset;
                for (std::size_t index = 0; index < m_branches.size(); ++index)
                {
                    const std::size_t oldQueSize = actions.size();
                    ingredients.Clean();

                    Variable* v = m_branches[index];
                    if (v->Scan(script, ingredients, actions, ctx, err))
                    {
                        ingredients.constraint = index;
                        return PushAction(actions, script, ingredients, ctx, err);
                    }
                    else
                    {
                        actions.resize(oldQueSize);
                        ingredients.GoBack(start); // 每次失败都必须回到最初的起点
                    }
                }

                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxBranch* var = allocator.ForceAlloc<SyntaxBranch>(std::move(m_branches));
                this->~SyntaxBranch();
                return var;
            }

        protected:
            std::vector<Variable*>  m_branches;
        }; // class SyntaxBranch

        class SyntaxLoop : public Variable
        {
        public:
            enum : std::size_t
            {
                Max = ~std::size_t(0)
            };

        public:
            SyntaxLoop(Variable* var, std::size_t min, std::size_t max)
                : m_var(var), m_min(min), m_max(max)
            {
                m_type.SetLoop();
            }

            bool Scan(const StrRef& script, Ingredients& ingredients, ActionQueue& actions, void* ctx, std::string& err) noexcept override
            {
                const std::size_t start = ingredients.offset;
                std::size_t cnt = 0;
                for (std::size_t i = 0; i < m_min; ++i, ++cnt)
                { // 最少循环 m_min 次
                    ingredients.Clean();
                    if (! m_var->Scan(script, ingredients, actions, ctx, err))
                    {
                        ingredients.GoBack(start); // 失败恢复到最原始的状态
                        return false;
                    }
                }

                for (std::size_t i = m_min; i < m_max; ++i, ++cnt)
                {
                    ingredients.Clean();
                    if (! m_var->Scan(script, ingredients, actions, ctx, err))
                    {
                        ingredients.GoBack(); // 不必回到最初起点
                        break;
                    }
                }

                if ((m_min <= cnt) && (cnt <= m_max))
                {
                    ingredients.start      = start;
                    ingredients.constraint = cnt;
                    return PushAction(actions, script, ingredients, ctx, err);
                }
                return false;
            }

            virtual Variable* _Move(VariableAllocator& allocator) noexcept override
            {
                SyntaxLoop* var = allocator.ForceAlloc<SyntaxLoop>(m_var, m_min, m_max);
                this->~SyntaxLoop();
                return var;
            }

        protected:
            Variable*   m_var = nullptr;
            std::size_t m_min = 0;
            std::size_t m_max = 0;
        }; // class SyntaxLoop

        ///////////////////////////////////////////////////////////////

        // 内建标识符
        class BuiltinIdent : public SyntaxToken
        { // [_a-zA-Z][_a-zA-Z0-9]*
        public:
            using SyntaxToken::SyntaxToken;

        private:
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string& err) noexcept override
            { // [_a-z-A-Z][_0-9a-zA-Z]*// 最小要求是一个字符
                std::size_t curOffset = offset;
                if (curOffset >= script.Length())
                {
                    err = _Name().ToStdString() + ": atleast one bytes.....";
                    return false;
                }

                const char first = script[curOffset++];

                if (!(('_' == first) 
                    || (first >= 'a' && first <= 'z')
                    || (first >= 'A' && first <= 'Z')))
                {
                    return false;
                }

                for (; curOffset < script.Length(); ++curOffset)
                {
                    const char ch = script[curOffset];
                    if (!('_' == ch
                        || (ch >= 'a' && ch <= 'z')
                        || (ch >= 'A' && ch <= 'Z')
                        || (ch >= '0' && ch <= '9')))
                    {
                        //err = _Name().ToStdString() + ": Error";
                        //return false;
                        break;
                    }
                }

                offset = curOffset;
                return true;
            }
        }; // class BuiltinIdent

        class BuiltinWhite : public SyntaxToken
        {
        public:
            // 白字符不作为 Token 
            BuiltinWhite() { m_type.Reset(); }

            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string&) noexcept override
            {
                if (offset < script.Length() && IsWhite(script[offset]))
                {
                    ++offset;
                    return true;
                }
                return false;
            }

        public:
            static bool IsWhite(char c) noexcept
            { // \t \n \v \f \r ' '
                switch (c)
                {
                case '\x09': case '\x0a': case '\x0b':
                case '\x0c': case '\x0d': case '\x20':
                    return true;
                default:
                    return false;
                }
            }
        }; // class BuiltinWhite

        class BuildinComment : public SyntaxToken
        { // 注释, 目前仅支持 #.*?(\n|$)

        public:
            // 注释不作为 Token 
            BuildinComment() { m_type.Reset(); }

            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string&) noexcept override
            {
                if (offset < script.Length() && ('#' == script[offset]))
                {
                    ++offset;
                    while (offset < script.Length())
                    {
                        if ('\n' == script[offset])
                        {
                            ++offset;
                            return true;
                        }
                        ++offset;
                    }

                    return true;
                }
                return false;
            }
        }; // class BuildinComment

        class BuiltinPunct : public SyntaxToken
        { // ispunct : !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~ 
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string&) noexcept override
            {
                if (offset < script.Length())
                {
                    return IsPunct(script[offset]);
                }
                return false;
            }
        public:
            static bool IsPunct(char c) noexcept
            {
                return !!std::ispunct((uint8_t)(c));
            }
        }; // class BuiltinPunct

        class BuiltinTokenRightZeroWidthAssertion : public SyntaxToken
        { // Token 右边界的零宽断言, 不会产生任何副作用
        public:
            BuiltinTokenRightZeroWidthAssertion() { m_type.Reset(); } // 零宽断言不能作为 Token

            // $|\s|[\x21–\x2F\x3A–\x40\x5B–\x60\x7B–\x7E]
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string&) noexcept override
            {
                const std::size_t _offset = offset; // 不消耗字符
                if (_offset == script.Length())
                {
                    return true;
                }

                if (_offset < script.Length())
                {
                    char c = script[_offset];
                    return BuiltinWhite::IsWhite(c) || BuiltinPunct::IsPunct(c);
                }

                return false;
            }
        };

        class BuiltinNaturalNumDec : public SyntaxToken
        { // [1-9][0-9]*|0
        public:
            using SyntaxToken::SyntaxToken;

            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t& data, std::string& err) noexcept override
            { // 十进制自然数
                std::size_t curOffset = offset;
                if (curOffset < script.Length())
                {
                    data = 0;

                    if (IsZero(script[curOffset])) 
                    { 
                        ++curOffset; 
                        offset = curOffset; 
                        return true; 
                    }

                    if (Is1To9(script[curOffset]))
                    {
                        data = data * 10u + ((uint64_t)(script[curOffset]) - (uint64_t)'0');
                        ++curOffset;

                        while (curOffset < script.Length())
                        {
                            if (Is0To9(script[curOffset]))
                            {
                                data = data * 10u + ((uint64_t)(script[curOffset]) - (uint64_t)'0');
                                ++curOffset;
                            }
                            else
                            {
                                break;
                            }
                        }
                        offset = curOffset;
                        return true;
                    }
                }
                return false;
            }

            bool IsZero(char c) const { return '0' == c; }
            bool Is1To9(char c) const { return '1' <= c && c <= '9'; }
            bool Is0To9(char c) const { return IsZero(c) || Is1To9(c); }
        }; // class BuiltinNaturalNumDec

        // 内置终结符的表达方式：单引号，双引号和//串
        class BuiltinTerminator : public SyntaxToken
        {
        public:
            using SyntaxToken::SyntaxToken;

            // 1. 单引号不支持转义，2. 双引号支持转义 3. 支持正则表达式
            bool ScanImpl(const StrRef& script, std::size_t& offset, uint64_t&, std::string& err) noexcept override
            {
                // 1. 单引号：      '[^']+
                // 2. 双引号：      暂不实现
                // 3. 正则表达式：  /.../ 支持空串
                std::size_t curOffset = offset;
                if (curOffset < script.Length())
                {
                    if ('\'' == script[curOffset])
                    {
                        ++curOffset;
                        while (curOffset < script.Length())
                        {
                            if ('\'' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        }
                    }
                    else if ('/' == script[curOffset])
                    { // 正则表达式， 不能出现不打印字符
                        ++curOffset;
                        while (curOffset < script.Length())
                        {
                            if (! std::isprint((uint8_t)(script[curOffset])))
                            {
                                err = "Unprintable characters cannot appear in regex"; // offset 也要带出去
                                return false;
                            }
                            
                            if ('\\' == script[curOffset])
                            { // 转义符跳过
                                ++curOffset;
                            }
                            if ('/' == script[curOffset++])
                            {
                                offset = curOffset;
                                return true;
                            }
                        } // end while (curOffset < script.Length())
                    } // end if '/'
                }
                return false;
            }
        }; // class BuiltinTerminator
       
        //////////////////////////////////
        // BulitinActions:
#undef _BULITIN_ACTIONS_DEFINE_BEGIN
#undef _BULITIN_ACTIONS_DEFINE_END
#define _BULITIN_ACTIONS_DEFINE_BEGIN(class_name) class class_name : public Action { 
#define _BULITIN_ACTIONS_DEFINE_END(class_name)   }

        _BULITIN_ACTIONS_DEFINE_BEGIN(FunctorAction)
        public:
            template <class Handler>
            explicit FunctorAction(Handler&& handler) : m_handler(std::forward<Handler>(handler)) 
            {
                assert(m_handler);
            }

        protected:
            bool Handler(const Food& food, void* ctx, std::string& err) override
            {
                assert(m_handler);
                return m_handler(food, ctx, err);
            }

            std::function<bool(const Food& food, void* ctx, std::string&)> m_handler;
        _BULITIN_ACTIONS_DEFINE_END(FunctorAction);

        template <class Vec>
        static inline void PureVec(Vec& vec) noexcept
        {
            vec.clear();
            vec.shrink_to_fit();
        }

        struct Context
        {
            Context(Variable*& userRootRef, VariableTable& varTable, VariableAllocator& varAlloc,
                MemoryAllocator& memAlloc, ActionTable& acTable)
                : m_userRootRef(userRootRef), m_varTable(varTable), m_varAlloc(varAlloc)
                , m_memAlloc(memAlloc), m_varActionTable(acTable)
            {}

            void ResetStack() noexcept
            {
                //PureVec(m_stackHead);
                //PureVec(m_stackExpr);

#pragma warning("PureVec")
            }

            struct HeadEle
            {
                StrRef      rawRef;
                std::string strData;  // 低于 C++17 不支持透明Hash 也不支持 std::string_view
            
                HeadEle() = default;
                HeadEle(const StrRef& r, std::string&& s) : rawRef(r), strData(std::move(s)) {}
            }; // struct HeadEle

            // LazyCtx
            std::vector<SyntaxToken*>                         m_stackTerminator;
            std::vector<StrRef>                               m_stackIdent;
            std::vector<HeadEle>                              m_stackHead;
            std::vector<Variable*>                            m_stackVar;
            std::vector<Variable*>                            m_stackOperand;
            std::vector<Variable*>                            m_stackStructExpr;
            std::vector<Variable*>                            m_stackExpr;
            std::vector<uint64_t>                             m_stackNumDec;
            std::vector<std::pair<std::size_t, std::size_t>>  m_stackLoopSymbol;
            std::vector<std::size_t>                          m_stackLoopSymbolOpt;
            std::vector<Variable*>                            m_stackLoopExpr;
            std::vector<Variable*>                            m_stackSeqExpr;
            std::vector<std::size_t>                          m_stackBranchExprSome;

            // RealtimeCtx
            std::unordered_set<std::string>                   m_realtimeHead;

            // Temp
            std::unordered_map<std::string, SyntaxToken*>   m_tokenTable; // 对 Token(字面值) 去重
            
            // 
            Variable*&                                      m_userRootRef;
            VariableTable&                                  m_varTable;
            VariableAllocator&                              m_varAlloc;
            MemoryAllocator&                                m_memAlloc;
            const ActionTable&                              m_varActionTable;
        }; // Context

        static inline Context* Ctx(void* ctx) noexcept
        {
            assert(nullptr != ctx);
            return static_cast<Context*>(ctx);
        }

        // BuiltinTerminatorAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinTerminatorAc)
        bool Handler(const Food& food, void* ctx, std::string&) override
        { // Terminator 分三种： ''_   ""_  //_
            assert(food.Length() >= 2u);
            SyntaxToken* token = nullptr;
            switch (food.Str()[0])
            {
            case '\'': // 注意这里可能会存在 后缀！！！!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                {
                    StrRef content(food.Begin() + 1, food.End() - 1);
                    token = GetToken(content.ToStdString(), content, ctx);
                }
                break;
            case '"': // @TODO ！！！ 注意这里要转义 ，而且还有后缀问题 ！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
                { 
                    StrRef content(food.Begin() + 1, food.End() - 1);
                    std::string realContent = content.ToStdString(); // <-- 转义在这里完成

                    char* buf = Ctx(ctx)->m_memAlloc.Alloc(realContent.size());
                    std::memmove(buf, realContent.data(), realContent.size());
                    StrRef holder(buf, realContent.size());
                    token = GetToken(realContent, holder, ctx);
                }
                break;
            case '/': // 正则表达式
                // @TODO 这里也有后缀问题！！！！

                break;
            default:
                return false;  // Error LogicBug
            }

            Ctx(ctx)->m_stackTerminator.push_back(token); // 没有名字
            return true;
        }

        SyntaxToken* GetToken(const std::string& realContent, const StrRef& holder, void* ctx) noexcept
        {
            SyntaxToken* token = nullptr;
            auto found = Ctx(ctx)->m_tokenTable.find(realContent); // 优先在缓存中寻找
            if (Ctx(ctx)->m_tokenTable.end() == found)
            {
                token = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxToken>(holder);
                Ctx(ctx)->m_tokenTable.emplace(realContent, token);
            }
            else
            {
                token = found->second;
            }
            assert(token);
            return token;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinTerminatorAc);

        // BuiltinIdentAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinIdentAc)
        bool Handler(const Food& food, void* ctx, std::string&) override
        {
            Ctx(ctx)->m_stackIdent.emplace_back(food.Begin(), food.End()); // StrRef 类型
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinIdentAc);
        
        ///  BuiltinHeadAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinHeadAc)
        bool RealTimeHandler(const Food& food, void* ctx, std::string& err) noexcept override
        { // 实时回调接口
            const StrRef& ident = food.Str();
            Ctx(ctx)->m_realtimeHead.emplace(ident.ToStdString()); // 覆盖失败的即可
            return true;
        }

        bool Handler(const Food&, void* ctx, std::string& err) noexcept override
        {
            StrRef headRef = Ctx(ctx)->m_stackIdent.back();
            Ctx(ctx)->m_stackIdent.pop_back();

            std::string head = headRef.ToStdString();
            auto found = Ctx(ctx)->m_varTable.find(head);
            if (Ctx(ctx)->m_varTable.end() == found  || found->second->IsMutable())
            {
                Ctx(ctx)->m_stackHead.emplace_back(headRef, std::move(head));
                return true;
            }

            err = "Variable '" + head + "' : Duplicate definition";
            return false;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinHeadAc);
        
        /// BuiltinVarAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinVarAc)
        bool Handler(const Food& food, void* ctx, std::string& err) noexcept override
        {
            std::string ident = Ctx(ctx)->m_stackIdent.back().ToStdString();
            Ctx(ctx)->m_stackIdent.pop_back();

            Variable* var = nullptr;
            auto found = Ctx(ctx)->m_varTable.find(ident);
            if (Ctx(ctx)->m_varTable.end() == found)
            {
                Variable* mut = Ctx(ctx)->m_varAlloc.ForceAlloc<MutableVariable>();
                if (nullptr == mut)
                { // Error
                    err = "Out of memory : ...";
                    return false;
                }
                Ctx(ctx)->m_varTable.emplace(ident, mut);
                var = mut;
            }
            else
            {
                var = found->second;
            }
            Ctx(ctx)->m_stackVar.push_back(var);
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinVarAc);

        ///  BuiltinOperandAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinOperandAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // terminator | func_expr | var
            switch (food.BranchID())
            {
            case 0:
                Ctx(ctx)->m_stackOperand.push_back(Ctx(ctx)->m_stackTerminator.back());
                Ctx(ctx)->m_stackTerminator.pop_back();
                return true;
            case 1:
#pragma warning("FuncExpr")
                //Ctx(ctx)->m_stackOperand.push_back(Ctx(ctx)->m_stackFuncExpr.back());
                //Ctx(ctx)->m_stackFuncExpr.pop_back();
                assert(0);
                return true;
            case 2:
                Ctx(ctx)->m_stackOperand.push_back(Ctx(ctx)->m_stackVar.back());
                Ctx(ctx)->m_stackVar.pop_back();
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinOperandAc);

        ///  BuiltinStructExprAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinStructExprAc);
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // operand | struct_with_bracket 
            switch (food.BranchID())
            {
            case 0:
                Ctx(ctx)->m_stackStructExpr.push_back(Ctx(ctx)->m_stackOperand.back());
                Ctx(ctx)->m_stackOperand.pop_back();
                return true;
            case 1:
                Ctx(ctx)->m_stackStructExpr.push_back(Ctx(ctx)->m_stackExpr.back());
                Ctx(ctx)->m_stackExpr.pop_back();
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinStructExprAc);

        ///  BuiltinNumDecAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinNumDecAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        { // 这里面的 Num 只支持 u64 类型 最大值 0xFFFFFFFFFFFFFFFF = 18446744073709551615
            if (food.Length() > 20)
            {
                return false;
            }
            else if (food.Length() == 20)
            { // @TODO 最大不能超过 18446744073709551615

            }

            Ctx(ctx)->m_stackNumDec.push_back(Food::AsInt(food.Data()));
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinNumDecAc);

        ///   BuiltinLoopSymbolAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopSymbolAc)
        bool Handler(const Food& food, void* ctx, std::string& err) noexcept override
        { // '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            switch (food.BranchID())
            {
            case 0: // ?
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(0u, 1u);
                return true;
            case 1: // *
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(0u, SyntaxLoop::Max);
                return true;
            case 2: // +
                Ctx(ctx)->m_stackLoopSymbol.emplace_back(1u, SyntaxLoop::Max);
                return true;
            case 3: // loop_n
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(n, n);
                }
                return true;
            case 4: // loop_m_n
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    std::size_t m = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();

                    if (m > n)
                    {
                        err = "Illegal Range { " + std::to_string(m) +", " + std::to_string(n) + " }";
                        return false;
                    }

                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(m, n);
                }
                return true;
            case 5: // loop_m_max
                {
                    std::size_t n = Ctx(ctx)->m_stackNumDec.back();
                    Ctx(ctx)->m_stackNumDec.pop_back();
                    Ctx(ctx)->m_stackLoopSymbol.emplace_back(n, SyntaxLoop::Max);
                }
                return true;
            default:
                return false;
            }
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopSymbolAc);

        ///  BuiltinLoopSymbolOptAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopSymbolOptAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            Ctx(ctx)->m_stackLoopSymbolOpt.push_back(food.LoopCount());
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopSymbolOptAc);

        ///  BuiltinLoopExprAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinLoopExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t loopSymbolOpt = Ctx(ctx)->m_stackLoopSymbolOpt.back();
            Ctx(ctx)->m_stackLoopSymbolOpt.pop_back();
            assert(loopSymbolOpt <= 1u);

            Variable* structExpr = Ctx(ctx)->m_stackStructExpr.back();
            Ctx(ctx)->m_stackStructExpr.pop_back();

            if (0 == loopSymbolOpt)
            { // 
                Ctx(ctx)->m_stackLoopExpr.push_back(structExpr);
            }
            else
            { // 组建循环 struct :
                const std::pair<std::size_t, std::size_t>& loopCntPair =
                    Ctx(ctx)->m_stackLoopSymbol.back();
                Ctx(ctx)->m_stackLoopSymbol.pop_back();

                SyntaxLoop* loopExpr = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxLoop>(
                    structExpr, loopCntPair.first, loopCntPair.second);
                Ctx(ctx)->m_stackLoopExpr.push_back(loopExpr);
            }

            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinLoopExprAc);

        ///  BuiltinSeqExprAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinSeqExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t seqEleCnt = food.LoopCount();
            assert(seqEleCnt <= Ctx(ctx)->m_stackLoopExpr.size());
            assert(0 != seqEleCnt); // Necessary ? 

            if (1u == seqEleCnt)
            {
                Ctx(ctx)->m_stackSeqExpr.push_back(Ctx(ctx)->m_stackLoopExpr.back());
                Ctx(ctx)->m_stackLoopExpr.pop_back();
            }
            else
            {
                const std::size_t size = Ctx(ctx)->m_stackLoopExpr.size();
                assert(seqEleCnt <= size);
                auto begin = Ctx(ctx)->m_stackLoopExpr.end() - seqEleCnt;
                auto end = Ctx(ctx)->m_stackLoopExpr.end();

                SyntaxSequence* seqExpr = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxSequence>(_range_arg, begin, end);
                Ctx(ctx)->m_stackSeqExpr.push_back(seqExpr);
                Ctx(ctx)->m_stackLoopExpr.resize(size - seqEleCnt);
            }
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinSeqExprAc);

        ///  BuiltinBranchExprSomeAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinBranchExprSomeAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            Ctx(ctx)->m_stackBranchExprSome.push_back(food.LoopCount());
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinBranchExprSomeAc);

        ///  BuiltinBranchPairsAc = BuiltinExprAc  
        ///    expr : branch_paris;
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinExprAc)
        bool Handler(const Food& food, void* ctx, std::string&) noexcept override
        {
            std::size_t cnt = Ctx(ctx)->m_stackBranchExprSome.back() + 1u;
            Ctx(ctx)->m_stackBranchExprSome.pop_back();

            if (1 == cnt)
            {
                Ctx(ctx)->m_stackExpr.push_back(Ctx(ctx)->m_stackSeqExpr.back());
                Ctx(ctx)->m_stackSeqExpr.pop_back();
            }
            else
            {
                const std::size_t size = Ctx(ctx)->m_stackSeqExpr.size();
                assert(cnt <= size);
                auto begin = Ctx(ctx)->m_stackSeqExpr.end() - cnt;
                auto end = Ctx(ctx)->m_stackSeqExpr.end();

                SyntaxBranch* branchPairs = Ctx(ctx)->m_varAlloc.ForceAlloc<SyntaxBranch>(_range_arg, begin, end);
                Ctx(ctx)->m_stackExpr.push_back(branchPairs);
                Ctx(ctx)->m_stackSeqExpr.resize(size - cnt);
            }
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinExprAc);

        ///  BuiltinProductionStatementAc
        _BULITIN_ACTIONS_DEFINE_BEGIN(BuiltinProductionStatementAc)
        bool Handler(const Food& food, void* ctx, std::string& err) noexcept override
        {
            Variable* body = Ctx(ctx)->m_stackExpr.back();
            Ctx(ctx)->m_stackExpr.pop_back();

            auto&& _head = Ctx(ctx)->m_stackHead.back();
            StrRef      headRef = _head.rawRef;
            std::string headData = std::move(_head.strData);
            Ctx(ctx)->m_stackHead.pop_back();

            auto foundVar = Ctx(ctx)->m_varTable.find(headData);
            if (Ctx(ctx)->m_varTable.end() == foundVar)
            {
                if (0 != body->_Name().IsNull() && body->GetAction())
                {// 比如：
                    // ident : $Ident(); # ident 挂了一个 Action
                    // var : ident;      # var 也挂了一个 Action, 那么此时 var 应该是一个 mut_var
                    auto acFound = Ctx(ctx)->m_varActionTable.find(headData);
                    if (Ctx(ctx)->m_varActionTable.end() != acFound)
                    { // head 也挂了一个 Action
                        body = Ctx(ctx)->m_varAlloc.ForceAlloc<MutableVariable>(body); // new body
                        assert(body->IsMutable());
                    }
                }
                Ctx(ctx)->m_varTable.emplace(headData, body); // 直接绑定关系 (直接引用)
            }
            else
            {
                Variable* var = foundVar->second;
                if (!var->IsMutable())
                {
                    err = "BUG xcxxxcxvsefwefwefwefwefwefwefwefwfwefwefwef\n";
                    return false;
                }
#pragma warning("  使得 var 的层级最小，（遇到 Action 终止优化）")
                //if (var 的 mut 只有一层)
                {
                    var->_SwapMut(body);
                    body = var;
                }
            }

            body->_SetName(headRef); // head 原始文本
            if (headData.compare("root"))
            { // 便携式 root
                Ctx(ctx)->m_userRootRef = body;
            }

            auto foundAction = Ctx(ctx)->m_varActionTable.find(headData);
            if (Ctx(ctx)->m_varActionTable.end() != foundAction)
            { // 此变量被绑定了 Action
                assert(nullptr == body->GetAction());
                assert(nullptr != foundAction->second);
                body->SetAction(foundAction->second);
            }
            return true;
        }
        _BULITIN_ACTIONS_DEFINE_END(BuiltinProductionStatementAc);

#undef _BULITIN_ACTIONS_DEFINE_BEGIN
#undef _BULITIN_ACTIONS_DEFINE_END
} // namespace internal

    // class Lexer
    class Lexer final
    {
        typedef Lexer _Myt;
        typedef std::deque<Action*> ActionsHolders;
    public:
        Lexer() { MakeInternal(); }

        ~Lexer() noexcept
        {
            DestoryActions(m_userActions);
            DestoryActions(m_innerActions);
        }

        // 清除用户数据
        void ResetDatabase() noexcept
        {
            ResetUserGraph();
            DestoryActions(m_userActions);
            m_userActionTable.clear();
            m_userVarAllocator.Reset();
            m_userMemAllocator.Reset();
        }

        // 线程无关
        bool SetGrammar(StrRef script, std::string& err) noexcept
        {
            err.clear();
            ResetUserGraph();

            ActionQueue aq; // 不能将其合并到 Ctx 中
            PreAllocAcQueue(aq, m_innerActions.size());
            internal::Context ctx(m_userRoot, m_userVariablesTable, 
                m_userVarAllocator, m_userMemAllocator, m_userActionTable); 
            if (ScanScript(script, *m_innerRoot, aq, &ctx, err))
            {
                return LazyCalc(aq, &ctx, err);
            }

            return false;
        }

        Variable* GetVariable(const std::string& varName) const noexcept
        {
            return FindVariable(varName);
        }

        bool Parse(StrRef script, void* ctx, std::string& err) noexcept
        {
            return Parse(m_userRoot, script, ctx, err);
        }

        bool Parse(Variable* root, StrRef script, void* ctx, std::string& err) noexcept
        {
            err.clear();
            if (nullptr != root)
            {
                ActionQueue aq;  
                PreAllocAcQueue(aq, m_userActions.size());
                if (ScanScript(script, *root, aq, ctx, err))
                {
                    return LazyCalc(aq, ctx, err);
                }

                return false;
            }

            err = "illegal variable: `root`";
            return false;
        }

        // 销毁由 Alloc1Action() 接口创建的 UserAction
        void ClearAction() noexcept { DestoryActions(m_userActions); }

        //  该接口用来创建用户自定义的 Action, 从此接口创建的 Action，对象析构是自动的
        template <class UserAction, class... Args,
            class = typename std::enable_if<std::is_base_of<Action, UserAction>::value>::type >
        Action* AllocAction(Args&&... args) noexcept
        {
            return _Myt::AllocAction<UserAction>(m_userActions, std::forward<Args>(args)...);
        }

        template <class UserAction, class... Args,
            class = typename std::enable_if<std::is_base_of<Action, UserAction>::value>::type >
        bool Bind(const std::string& userVar, Args&&... args) noexcept
        {
            return BindImpl(userVar, [&]() -> Action* {
                return AllocAction<UserAction>(std::forward<Args>(args)...);
            });
        }

        bool Bind(const std::string& userVar, Action* action) noexcept
        {
            return BindImpl(userVar, [action]() -> Action* { return action; });
        }

        template <class Handler>
        bool Bind(const std::string& userVar, Handler&& handler) noexcept
        {
            return Bind<internal::FunctorAction>(userVar, std::forward<Handler>(handler));
        }

    private:
        bool LazyCalc(ActionQueue& aq, void* ctx, std::string& err) const noexcept
        {
            std::size_t index = 0; // just for debug
            for (ActionQueueEle& ac : aq)
            {
                if (! ac.action->Handler(ac.food, ctx, err))
                {
                    return false;
                } ++index; (void)index;
            }
            return true;
        }

        static bool ScanScript(const StrRef& script, Variable& root ,
            ActionQueue& aq, void* ctx, std::string& err)  noexcept
        {
            Ingredients ingredients;
            if (root.Scan(script, ingredients, aq, ctx, err))
            {
                return script.Length() == ingredients.offset;
            }

            return false;
        }

        Variable* FindVariable(const std::string& userVarName) const noexcept
        {
            auto found = m_userVariablesTable.find(userVarName);
            if (m_userVariablesTable.end() != found)
            {
                return found->second;
            }
            return nullptr;
        }

        template <class MyAction, class... Args>
        static Action* AllocAction(ActionsHolders& actions, Args&&... args)
        {
            static_assert(std::is_base_of<Action, MyAction>::value, "Error");

            Action* action = new MyAction(std::forward<Args>(args)...);
            actions.push_back(action);
            return action;
        }

        template <class MyAction, class... Args>
        Action* AllocInternalAction(Args&&... args)
        {
            return _Myt::AllocAction<MyAction>(m_innerActions, std::forward<Args>(args)...);
        }

    private:
        template <class T, class... Args>
        Variable* AllocInnerDollarFn(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_innerVarAllocator.ForceAlloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                ret->_SetName(name);
            }
            return ret;
        }

        template <class T>
        Variable* AllocInnerDollarFn(StrRef name, StrRef token,
                Variable* whites = nullptr, Variable* rightAssert = nullptr) noexcept
        {
            return AllocInner<T>(name, token, whites, rightAssert);
        }

        template <class T, class... Args,
           class = typename std::enable_if<!std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* AllocInner(StrRef name, Args&&...args) noexcept
        {
            Variable* ret = m_innerVarAllocator.Alloc<T>(std::forward<Args>(args)...);
            if (nullptr != ret)
            {
                if (ret->_Name().IsNull())
                {
                    ret->_SetName(name);
                }
                else
                {
                    assert(0);  // <--------------- debug 
                    return nullptr;
                }
            }
            return ret;
        }

        template <class T, 
            class = typename std::enable_if<std::is_base_of<internal::SyntaxToken, T>::value>::type>
        Variable* AllocInner(StrRef name, StrRef token,
            Variable* whites = nullptr, Variable* rightAssert = nullptr) noexcept
        {
            internal::SyntaxToken* ret 
                = (internal::SyntaxToken*)m_innerVarAllocator.ForceAlloc<T>(token);
            if (nullptr != ret)
            {
                ret->_SetName(name);
                if (! ret->SetLeftWhites(whites))
                {
                    return nullptr;
                }

                ret->SetRightAssert(rightAssert);
            }
            return ret;
        }

        //// internal: 内置语法
        bool MakeInternal() noexcept
        {
            using namespace internal;

            Variable* white   = AllocInnerDollarFn<BuiltinWhite>("white");
            Variable* comment = AllocInnerDollarFn<BuildinComment>("comment");
            Variable* whites  = AllocInner<SyntaxLoop>("whites",
                AllocInner<SyntaxBranch>("white_or_comment", white, comment), 0, SyntaxLoop::Max);
            Variable* tokenRightAssert = AllocInnerDollarFn<BuiltinTokenRightZeroWidthAssertion>("tokenRightAssert");

            // ident      : ; # func  (暂时使用函数)
            Variable* ident = AllocInnerDollarFn<BuiltinIdent>("ident", StrRef(""), whites); // ident 不需要右边界断言
            // head  :  ident; # 正常情况下，先挂 Action, 所以这里知道 head 是 mut 还是直接引用
            Variable* head = AllocInner<MutableVariable>("head", ident); // 未定义的变量名 
            // var   :  ident; # 同上
            Variable* var  = AllocInner<MutableVariable>("var", ident);  // 已经定义的变量名

            // terminator    : '.*?'  # 或者正则表达式 
            Variable* terminator = AllocInnerDollarFn<BuiltinTerminator>("terminator", StrRef(""), whites);

            // operand : terminator | func_expr | var;
            Variable* func_expr = AllocInner<SyntaxSequence>("func_expr"); // decl
            Variable* operand = AllocInner<SyntaxBranch>("operand", terminator, func_expr, var);

            // expr : 优先级 循环 > 序列 > 分支
            Variable* $0x7B = AllocInner<SyntaxToken>("{", "{", whites);
            Variable* $0x7D = AllocInner<SyntaxToken>("}", "}", whites);
            Variable* $0x2C = AllocInner<SyntaxToken>(",", ",", whites);
            Variable* $0x28 = AllocInner<SyntaxToken>("(", "(", whites);
            Variable* $0x29 = AllocInner<SyntaxToken>(")", ")", whites);

            // decl expr;
            Variable* expr = AllocInner<MutableVariable>("expr"); // mut_var

            // struct_with_bracket : '(' expr ')' ;
            Variable* struct_with_bracket = AllocInner<SyntaxSequence>("struct_with_bracket", $0x28, expr, $0x29);

            // struct_expr : operand | struct_with_bracket # 这里 仅能容纳 var ??? 那正则表达式怎么办 ？？？TODO,  可以使用变量！！！！
            Variable* struct_expr = AllocInner<SyntaxBranch>("struct_expr", operand, struct_with_bracket);

            // num_dec regex : [1-9][0-9]*|0 ;
            Variable* num_dec = AllocInnerDollarFn<BuiltinNaturalNumDec>("num_dec", StrRef(""), whites, tokenRightAssert);

            Variable* loop_n = AllocInner<SyntaxSequence>("loop_n", $0x7B, num_dec, $0x7D);
            // loop_m_n   : '{' num_dec ',' num_dec '}';
            Variable* loop_m_n = AllocInner<SyntaxSequence>("loop_m_n",
                $0x7B, num_dec, $0x2C, num_dec, $0x7D);
            // loop_m_max : '{' num_dec ',' '}';
            Variable* loop_m_max = AllocInner<SyntaxSequence>("loop_m_max", $0x7B, num_dec, $0x2C, $0x7D);

            // loop_symbol     : '?' | '*' | "+" | loop_n | loop_m_n | loop_m_max ;
            Variable* loop_symbol = AllocInner<SyntaxBranch>("loop_symbol",
                AllocInner<SyntaxToken>("?", "?", whites),
                AllocInner<SyntaxToken>("*", "*", whites),
                AllocInner<SyntaxToken>("+", "+", whites),
                loop_n, loop_m_n, loop_m_max);
            // loop_symbol_opt : loop_symbol ?
            Variable* loop_symbol_opt = AllocInner<SyntaxLoop>("loop_symbol_opt", loop_symbol, 0, 1u);
            // loop_expr : struct_expr loop_symbol_opt;
            Variable* loop_expr = AllocInner<SyntaxSequence>("loop_expr", struct_expr, loop_symbol_opt);

            // seq_expr : loop_expr +
            Variable* seq_expr = AllocInner<SyntaxLoop>("seq_expr", loop_expr, 1u, SyntaxLoop::Max);

            // branch_expr : '|' seq_expr;
            Variable* branch_expr = AllocInner<SyntaxSequence>("branch_expr", AllocInner<SyntaxToken>("|", "|", whites), seq_expr);
            // seq_expr_some : branch_expr *;
            Variable* branch_expr_some = AllocInner<SyntaxLoop>("branch_expr_some", branch_expr, 0, SyntaxLoop::Max);
            // branch_pairs   : branch_expr branch_expr_some
            Variable* branch_pairs = AllocInner<SyntaxSequence>("branch_pairs", seq_expr, branch_expr_some);

            // expr      ： branch_pairs;
            assert(expr->IsMutable());
            expr->_SwapMut(branch_pairs);

            // productionStatement :  head expr ';' ; 
            Variable* production_statement = AllocInner<SyntaxSequence>("production_statement", head, AllocInner<SyntaxToken>(":", ":", whites), expr);

            // semicolon_opt : ','?;
            Variable* semicolon_opt = AllocInner<SyntaxLoop>("semicolon_opt", $0x2C, 0u, 1u);

            Variable* let_keyword = AllocInner<SyntaxToken>("let_keyword", "let", whites, tokenRightAssert);
            Variable* equal_mark = AllocInner<SyntaxToken>("=", "=", whites);
            Variable* where_keyword = AllocInner<SyntaxToken>("where", "where", whites, tokenRightAssert);

            // operand_swap : operand '->' operand;
            Variable* operand_swap = AllocInner<SyntaxSequence>("operand_swap", operand, AllocInner<SyntaxToken>("->", "->", whites), operand);
            // operand_swap_some:  (semicolon_opt operand_swap)*;
            Variable* operand_swap_some = AllocInner<SyntaxLoop>("operand_swap_some",
                AllocInner<SyntaxSequence>("another_operand_swap", semicolon_opt, operand_swap),
                0, SyntaxLoop::Max);


            //// let var = var2, where a -> b, c -> d; // @TODO 这里是否要求 a 和 c 必须是非终结符或指向终结符的变量？ !!!!!!!!!!!!!! 后者无歧义？？？
            // let_where : 'let' head '=' var ','? 'where' operand '->' operand (','? operand '->' operand)*
            Variable* let_where = AllocInner<SyntaxSequence>("let_where", let_keyword, head, equal_mark, var,
                semicolon_opt, where_keyword, operand_swap, operand_swap_some);

            // let var = $func();
            // func_expr: 'let' var '=' /$func\s*(/ (operand(',' operand)*) ? ')';
            Variable* local_func_prefix = AllocInner<SyntaxToken>("local_func_prefix", "$", whites);
            Variable* func_name = AllocInner<SyntaxSequence>("func_name", local_func_prefix, ident);
            Variable* func_arg = AllocInner<SyntaxBranch>("func_arg", num_dec, operand);
            Variable* suffix_args = AllocInner<SyntaxSequence>("suffix_args", $0x2C, func_arg);
            Variable* suffix_args_some = AllocInner<SyntaxLoop>("suffix_args_some", suffix_args, 0, SyntaxLoop::Max);
            Variable* func_args = AllocInner<SyntaxSequence>("func_args", func_arg, suffix_args_some);
            Variable* func_args_opt = AllocInner<SyntaxLoop>("func_args_opt", func_args, 0, 1u);
            //Variable* func_expr = AllocInner<SyntaxSequence>("func_expr", func_name, $0x28, func_args_opt, $0x29);
            assert(func_expr->Type().IsSequence());
            ((SyntaxSequence*)func_expr)->AppendVariable(func_name).AppendVariable($0x28).AppendVariable(func_args_opt).AppendVariable($0x29);

            // statement : production_statement | let_where ;
            Variable* statement = AllocInner<SyntaxBranch>("statement", production_statement, let_where);

            // statement_with_endmark : statement ';'+;
            Variable* statement_with_endmark = AllocInner<SyntaxSequence>("statement_with_endmark", statement,
                AllocInner<SyntaxLoop>("statement_endmark", AllocInner<SyntaxToken>(";", ";", whites), 1u, SyntaxLoop::Max), whites);

            // root:         statement_with_endmark + ;
            m_innerRoot = AllocInner<SyntaxLoop>("root", statement_with_endmark, 1u, SyntaxLoop::Max);
            if (nullptr == m_innerRoot)
            {
                Error("Fatal error:  Unable to initialize 'root' !");
            }

            /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Action
            SafeBind(terminator, AllocInternalAction<BuiltinTerminatorAc>()) || Error("BUG: bind 'terminator' action");
            SafeBind(ident, AllocInternalAction<BuiltinIdentAc>())           || Error("BUG: bind 'ident' action");
            SafeBind(head, AllocInternalAction<BuiltinHeadAc>())             || Error("BUG: bind 'head' action");
            SafeBind(var, AllocInternalAction<BuiltinVarAc>())               || Error("BUG: bind 'var' action");
            SafeBind(operand, AllocInternalAction<BuiltinOperandAc>())       || Error("BUG: bind 'operand' action");
            SafeBind(struct_expr, AllocInternalAction<BuiltinStructExprAc>())|| Error("BUG: bind 'struct_expr' action");
            SafeBind(num_dec, AllocInternalAction<BuiltinNumDecAc>())        || Error("BUG: bind 'num_dec' action");
            SafeBind(loop_symbol, AllocInternalAction<BuiltinLoopSymbolAc>())|| Error("BUG: bind 'loop_symbol' action");
            SafeBind(loop_symbol_opt, AllocInternalAction<BuiltinLoopSymbolOptAc>()) || Error("BUG: bind 'loop_symbol_opt' action");
            SafeBind(loop_expr, AllocInternalAction<BuiltinLoopExprAc>())    || Error("BUG: bind 'loop_expr' action");
            SafeBind(seq_expr, AllocInternalAction<BuiltinSeqExprAc>())      || Error("BUG: bind 'seq_expr' action");
            SafeBind(branch_expr_some, AllocInternalAction<BuiltinBranchExprSomeAc>()) || Error("BUG: bind 'branch_expr_some' action");
            SafeBind(expr, AllocInternalAction<BuiltinExprAc>())             || Error("BUG: bind 'expr' action");
            SafeBind(production_statement, AllocInternalAction<BuiltinProductionStatementAc>()) || Error("BUG: bind 'production_statement' action");
            return true;
        } // MakeInteranlSyntax

        template <class Handler>
        bool BindImpl(const std::string& var, Handler&& handler) noexcept
        {
            auto found = m_userActionTable.find(var);
            if (m_userActionTable.end() == found)
            {
                m_userActionTable.emplace(var, handler());
                return true;
            }

            return false;
        }

        bool SafeBind(Variable* var, Action* action) noexcept
        {
            if (nullptr == var || nullptr == action)
            {
                return false;
            }

            if (nullptr == var->GetAction())
            {
                var->SetAction(action);
                return true;
            } 
            return false;
        }

        static bool Error(StrRef err)
        {
            throw(err.ToStdString());
        }

        static void DestoryActions(ActionsHolders& actions) noexcept
        {
            for (Action* action : actions)
            {
                delete action;
            }
            actions.clear();
        }

        void PreAllocAcQueue(ActionQueue& aq, std::size_t cap) noexcept
        {
            aq.resize(cap); // aq.reserve(cap)
            aq.clear();
        }

        void ResetUserGraph() noexcept
        {
            m_userRoot = nullptr;
            m_userVariablesTable.clear();
        }

    private:
        Variable*              m_innerRoot;
        VariableAllocator      m_innerVarAllocator;  // 内部变量分配
        ActionsHolders         m_innerActions;       // 内置文法动作集合
        MemoryAllocator        m_innerMemAllocator;  // 内部内存分配

        // 以下是用户 DB
        Variable*              m_userRoot = nullptr;
        ActionsHolders         m_userActions;        // 用户文法动作集合 
        ActionTable            m_userActionTable;    // 用户申请的动作表 
        VariableAllocator      m_userVarAllocator;   // 用户变量分配
        MemoryAllocator        m_userMemAllocator;   // 用户内存分配
        VariableTable          m_userVariablesTable; // 输出(1) 
    }; // class Lexer

    //////////////////////////////////////////////
    // Impl
    template <class Allocator>
    void Variable::ReshapeToMut(Allocator& allocator) noexcept
    {
        if (!IsMutable())
        {
            Variable* var = _Move(allocator);

            new (this) internal::MutableVariable();
            this->m_mut = var;
        }
    }

} // namespace DDL_LEXER