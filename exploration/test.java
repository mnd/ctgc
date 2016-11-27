private IType[] findInterfaces(IType type)
{   Collection<IType> interfaces = new ArrayList<IType>();
    IIFinder[] finders = getImplementorFinders();
    
    for (int i = 0; i < finders.length; i++) {
	Collection<IType> types = 
           finders[i].findIn(type, this.Monitor);
	if (types != null) {
	    interfaces.addAll(types);
	}
    }    
    return interfaces.toArray(new IType[interfaces.size()]);
}
